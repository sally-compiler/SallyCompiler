#include "opt.h"

bool should_inline(Procedure_IR *f, uint32 call_count, Function_Call *call) {

    if (f->is_builtin || is_function_recursive(f)) {
        return false;
    }

    if (call->b->loop_depth == 0 && strcmp(call->caller->name, "main") == 0) {
        return false;
    }

    // inline functions that's only been called once
    if (call_count == 1) {
        return true;
    }

    // @TODO: adjust more carefully
    if (f->values_count < 60) {
        return true;
    }

    return false;
}

struct Inline_Info {
    Procedure_IR *caller = NULL;
    Procedure_IR *callee = NULL;
    Function_Call *call_inst = NULL;
    bool done = false;
};

// each function keeps a list of functions (inline info)
// that should be inlined into it
Map<Procedure_IR *, Array<Inline_Info>> func_inline_info;

// whether a function has inlined all the functions into it
Map<Procedure_IR *, bool> inline_done;

// @cleanup
void transfer_succs(Basic_Block *old_bb, Basic_Block *new_bb) {
    new_bb->succs.release();

    for (auto succ : old_bb->succs) {

        new_bb->succs.push(succ);
        int p = succ->preds.find(old_bb);
        succ->preds[p] = new_bb;

        for (auto v : succ->insts) {
            if (v->type == PHI || v->type == MEMORY_PHI) {
                auto phi = (Phi *)v;
                int p = phi->sources.find(old_bb);
                phi->sources[p] = new_bb;
            }
        }
    }
}

// find functions that are called exactly once
// and they must be called outside, thus excluding recusive functions
void find_functions_to_inline(Program_IR *prog) {

    // count how many times each function is called
    Map<Procedure_IR *, uint32> call_count;

    for (auto f : prog->procedures) {
        for (auto bb : f->blocks) {
            for (auto v : bb->insts) {
                auto call = v->as<Function_Call>();
                if (!call)
                    continue;

                call_count[f]++;
            }
        }
    }

    for (auto f : prog->procedures) {

        for (auto bb : f->blocks) {
            for (auto v : bb->insts) {
                auto call = v->as<Function_Call>();
                if (!call)
                    continue;

                if (!should_inline(call->f, call_count[f], call))
                    continue;

                Inline_Info info;
                info.caller = f;
                info.callee = call->f;
                info.call_inst = call;

                printf("%s should be inlined! (called by %s)\n", call->f->name,
                       info.caller->name);

                func_inline_info[info.caller].push(info);
            }
        }
    }
}

// @TODO: inline function that is called multiple times?
void inline_function(Inline_Info inline_info) {

    auto caller = inline_info.caller;
    auto callee = inline_info.callee;
    auto call = inline_info.call_inst;

    callee = copy_function(callee);

    // split bb into half at call site
    // @FIXME: update control transfre inst?
    auto before_call = call->b;
    auto after_call = new Basic_Block;
    after_call->index_in_procedure = caller->blocks.len;
    caller->blocks.push(after_call);
    transfer_succs(call->b, after_call);

    for (auto callee_bb : callee->blocks) {
        caller->blocks.push(callee_bb);
    }
    renumber_blocks(caller);

    auto call_index = before_call->insts.find(call);

    for (int i = call_index + 1; i < before_call->insts.len; i++) {
        auto I = before_call->insts[i];
        I->b = after_call;
        after_call->insts.push(I);
    }

    // remove call inst and insts after it
    before_call->insts.set_len(call_index);

    before_call->succs.release();
    connect_CFG(before_call, callee->start_block, true);
    connect_CFG(callee->exit_block, after_call, true);

    for (int a = 0; a < callee->arguments.len; a++) {
        auto arg_val = callee->arguments[a];

        // the argument is probably elminated by DCE
        if (arg_val == NULL) {
            continue;
        }

        auto v = call->arguments[a]->v;

        Ast_Declaration *decl = NULL;
        if (v->type == ARGUMENT)
            decl = v->as<Argument>()->decl;
        if (v->type == GLOBAL)
            decl = v->as<Global>()->decl;
        if (v->type == ALLOCA)
            decl = v->as<Alloca>()->decl;

        if (decl) {

            for (auto u = arg_val->use; u; u = u->next) {
                if (auto read = u->user->as<Memory_Read>()) {
                    if (read->base->v == arg_val) {
                        read->decl = decl;
                    }
                } else if (auto write = u->user->as<Memory_Write>()) {
                    if (write->base->v == arg_val) {
                        write->decl = decl;
                    }
                }
            }
        }

        arg_val->replace_all_uses_with(v);
        callee->start_block->insts.remove(arg_val);
    }

    Value *ret_val = NULL;

    // @Cleanup
    if (callee->exit_block->preds.len == 1) {
        auto pred = callee->exit_block->preds[0];
        auto ret_inst = pred->insts.back()->as<Instruction_Return>();
        assert(ret_inst);
        if (callee->has_return_value) {
            assert(ret_inst->return_value);
            ret_val = ret_inst->return_value->v;
        }
        pred->insts.pop(); // delete the return inst
        insert_br(pred, callee->exit_block);
        delete ret_inst;
    } else if (callee->has_return_value) {

        auto phi = new Phi(callee->exit_block);
        phi->n = -1; // remember to renumber
        for (auto pred : callee->exit_block->preds) {
            auto ret_inst = pred->insts.back()->as<Instruction_Return>();
            assert(ret_inst);
            assert(ret_inst->return_value);
            phi->operands.push(new_use(ret_inst->return_value->v, phi));
            ret_inst->drop_uses_of_operands();
            phi->sources.push(pred);
            pred->insts.pop();
            insert_br(pred, callee->exit_block);
            delete ret_inst;
        }

        callee->exit_block->insts.insert(0, phi);
        ret_val = (Value *)phi;
    }

    if (callee->has_return_value) {
        assert(ret_val);
        call->replace_all_uses_with(ret_val);
    }

    call->drop_uses_of_operands();

    renumber_values(caller);
    // compute_CFG(caller);
}

void inline_all_into_function(Procedure_IR *f) {

    if (inline_done[f])
        return;
    inline_done[f] = true;

    // inline callee functions first
    for (auto callee : f->callee) {
        inline_all_into_function(callee);
    }

    for (auto inline_info : func_inline_info[f]) {
        inline_function(inline_info);
    }
}

OPT_PASS(inline_functions) {
    compute_callgraph(IR);
    func_inline_info.clear();
    inline_done.clear();
    find_functions_to_inline(IR);

    for (auto f : IR->procedures) {
        inline_all_into_function(f);
    }
}
