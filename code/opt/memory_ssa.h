#include "opt.h"

// two decls don't alias if they have different dimensions
bool may_alias_by_dims(Ast_Declaration *x, Ast_Declaration *y) {

    if (x->decl_type->tag == TYPE_INTEGER ||
        y->decl_type->tag == TYPE_INTEGER) {
        return false;
    }

    auto x_dims = ((Type_Info_Array *)x->decl_type)->dimension;
    auto y_dims = ((Type_Info_Array *)y->decl_type)->dimension;

    if (x_dims.len != y_dims.len)
        return false;

    for (int i = 0; i < x_dims.len; i++) {
        if (i == 0 && (x_dims[0] == -1 || y_dims[0] == -1))
            continue;
        if (x_dims[i] != y_dims[i])
            return false;
    }

    return true;
}

bool may_alias(Ast_Declaration *x, Ast_Declaration *y) {

    // the whole memory
    if (x->decl_type == NULL || y->decl_type == NULL)
        return true;

    bool x_is_local = (!x->is_global && x->arg_index == -1);
    bool y_is_local = (!y->is_global && y->arg_index == -1);

    if (x->is_global) {
        if (y->is_global) {
            return x == y;
        } else if (y->arg_index != -1) {
            return may_alias_by_dims(x, y);
        } else {
            // y is a local, cant alias with a global
            return false;
        }
    } else if (x_is_local) {
        if (y_is_local) {
            return x == y;
        } else if (y->is_global) {
            return may_alias_by_dims(x, y);
        } else { // is argument
            // local and arg can alias in recursive functions
            // @TODO: optimize later
            // return true;
            return false;
        }
    } else { // is argument
        /* @note:
        if (y_is_local) false;
        if (y_is_global) if x is scalar false, otherwise dim analysis;
        if (y is arg) both are arg arrays, dim analysis.
        */
        if (y->is_global || y->arg_index != -1) {
            return may_alias_by_dims(x, y);
        } else if (y_is_local) {
            // assume local arrays doesn't alias with arg!!!
            return false;
            // return true; // see the @TODO above
        } else {
            return false;
        }
    }
}

// no numbering!
// make sure to renumber after calling this
void insert(Basic_Block *bb, Value *v, bool insert_to_first = false) {
    v->b = bb;
    if (insert_to_first) {
        bb->insts.insert(0, v);
    } else {
        bb->insts.push(v);
    }
}

void save_memory_version(Procedure_IR *f, Ast_Declaration *decl,
                         Basic_Block *block, Value *d);
Value *read_memory_version(Procedure_IR *f, Ast_Declaration *decl,
                           Basic_Block *block);

Array<Ast_Declaration *> current_live_decls;

Value *add_phi_operands(Procedure_IR *f, Ast_Declaration *decl, Memory_Phi *phi,
                        Basic_Block *block) {
    for (int i = 0; i < block->preds.len; i++) {
        auto operand = read_memory_version(f, decl, block->preds[i]);
        phi->operands.push(new_use(operand, phi));
        phi->sources.push(block->preds[i]);
    }

    insert(block, phi, true);
    return phi;
}

void seal_block(Procedure_IR *f, Basic_Block *block) {
    block->sealed = true;
    for (auto p : block->incomplete_phis) {
        add_phi_operands(f, p.first, (Memory_Phi *)p.second, block);
    }
    block->incomplete_phis.release();
}

Value *read_memory_version(Procedure_IR *f, Ast_Declaration *decl,
                           Basic_Block *block) {
    Value *ret_val = NULL;
    auto vermap = f->bb_vermap[block];
    for (int i = vermap.len - 1; i >= 0; i--) {
        auto it = vermap[i];
        if (may_alias(it.first, decl)) {
            ret_val = it.second;
            break;
        }
    }

    // read recursively in preds
    if (!ret_val) {

        if (!block->sealed) {
            auto mem_phi = new Memory_Phi(block);
            auto p = Pair<Ast_Declaration *, Value *>(decl, mem_phi);
            block->incomplete_phis.push(p);
            ret_val = mem_phi;
        } else if (block->preds.len == 1) {
            ret_val = read_memory_version(f, decl, block->preds[0]);
        } else {
            auto mem_phi = new Memory_Phi(block);
            save_memory_version(f, decl, block, mem_phi);
            ret_val = add_phi_operands(f, decl, mem_phi, block);
        }
    }

    assert(ret_val);
    if (block != f->start_block)
        save_memory_version(f, decl, block, ret_val);

    return ret_val;
}

Value *read_memory_version_bb(Procedure_IR *f, Ast_Declaration *decl,
                              Basic_Block *block) {
    Value *ret_val = NULL;
    auto vermap = f->bb_vermap[block];
    for (int i = vermap.len - 1; i >= 0; i--) {
        auto it = vermap[i];
        if (may_alias(it.first, decl)) {
            ret_val = it.second;
            break;
        }
    }

    return ret_val;
}

void save_memory_version(Procedure_IR *f, Ast_Declaration *decl,
                         Basic_Block *block, Value *d) {
    assert(d->type == MEMORY_DEF || d->type == MEMORY_PHI);

    f->trash[d][block][decl] = true;

    auto p = Pair<Ast_Declaration *, Value *>(decl, d);
    f->bb_vermap[block].push(p);

    if (current_live_decls.find(decl) == -1)
        current_live_decls.push(decl);
}

void visit_block(Procedure_IR *f, Basic_Block *bb) {

    for (int i = 0; i < bb->insts.len; i++) {
        auto v = bb->insts[i];
        if (auto store = v->as<Memory_Write>()) {
            auto def = new Memory_Def;
            def->cause = new_use(store, def);
            def->clobbers =
                new_use(read_memory_version(f, store->decl, bb), def);
            def->decl = store->decl;

            bb->insts.insert(i + 1, def);
            def->b = bb;
            save_memory_version(f, store->decl, bb, def);
        } else if (auto call = v->as<Function_Call>()) {
            // clobbers all memory versions
            // @TODO: very stupid approach, make it smarter
            // @TODO: some functions, like memset, are known to clobber only
            for (auto decl : current_live_decls) {
                auto def = new Memory_Def;
                def->decl = decl;
                def->cause = new_use(call, def);
                def->clobbers = new_use(read_memory_version(f, decl, bb), def);
                bb->insts.insert(i + 1, def);
                def->b = bb;
                save_memory_version(f, decl, bb, def);
            }
        } else if (auto load = v->as<Memory_Read>()) {
            auto mem_ver = read_memory_version(f, load->decl, bb);
            load->mem_ver = new_use(mem_ver, load);
        }
    }
}

void remove_mem_info(Procedure_IR *f) {
    // remove previous mem info
    f->bb_vermap.clear();
    f->trash.clear();
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (v->type == MEMORY_DEF || v->type == MEMORY_PHI) {
                v->mark_remove_from_bb();
                v->drop_uses_of_operands();
            }

            if (auto read = v->as<Memory_Read>()) {
                if (read->mem_ver) {
                    read->mem_ver->remove();
                    read->mem_ver = NULL;
                }
            }
        }
        bb->remove_dead_values();
    }
}

void build_function_mem_ssa(Procedure_IR *f) {
    current_live_decls.release();

    remove_mem_info(f);

    // put initial memory version in start block
    auto initial_mem_state = new Memory_Def;
    f->whole_memory = new Ast_Declaration;
    initial_mem_state->is_initial_version = true;
    insert(f->start_block, initial_mem_state, true);
    save_memory_version(f, f->whole_memory, f->start_block, initial_mem_state);

    // @FIXME: order is incorrect
    // we want to bfs ifs, and dfs whiles...
    // bfs each block, go thourgh values in them
    mark_all_blocks_unvisited(f->blocks);

    Queue<Basic_Block *> queue;
    queue.push(f->start_block);
    Array<Basic_Block *> unsealed_blocks;

    while (!queue.empty()) {
        auto bb = queue.front();
        queue.pop();

        if (bb_visited(bb))
            continue;

        // a bb can only be visited
        // after all its preds were visited
        // except for loop header
        bool stop_visit = false;
        if (!bb->is_loop_header) {
            for (auto pred : bb->preds) {
                if (!bb_visited(pred)) {
                    stop_visit = true;
                    break;
                }
            }
        }
        if (stop_visit)
            continue;

        mark_visited(bb);
        if (bb->is_loop_header) {
            unsealed_blocks.push(bb);
            bb->sealed = false;
        }
        visit_block(f, bb);

        if (bb->insts.len == 0)
            continue;
        auto last_inst = bb->insts[bb->insts.len - 1];

        if (auto br = last_inst->as<Instruction_Branch>()) {
            queue.push(br->true_target);
            queue.push(br->false_target);
        } else if (auto jmp = last_inst->as<Instruction_Direct_Branch>()) {
            queue.push(jmp->target);
        }
    }

    for (int i = unsealed_blocks.len - 1; i >= 0; i--) {
        seal_block(f, unsealed_blocks[i]);
    }

    renumber_values(f);
    remove_trivial_phi(f);
}

OPT_PASS(memory_ssa) {
    run_on_every_function(IR, build_function_mem_ssa);
    compute_callgraph(IR);
}
