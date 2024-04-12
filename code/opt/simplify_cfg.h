#include "opt.h"

// @TODO: merge blocks

namespace SIMPLIFY_CFG {

void simplify_on_single_function(Procedure_IR *f) {

    /*
    // remove dead blocks
    Array<Basic_Block *> cleaned_blocks;
    for (auto bb : f->blocks) {
        if (!is_block_dead(f, bb)) {
            bb->index_in_procedure = cleaned_blocks.len;
            cleaned_blocks.push(bb);
        }
    }

    // @TODO: better way to transfer ownership
    // can implement operator=() for Arrays
    f->blocks = cleaned_blocks;
    */

    // remove dead branch (branching on a constant)
    // by replacing Insturction_Branch with Instruction_Direct_Branch
    for (auto bb : f->blocks) {
        if (bb->insts.len != 0) {
            auto last_inst = bb->insts[bb->insts.len - 1];
            if (last_inst->type == INST_BRANCH) {
                auto br = (Instruction_Branch *)last_inst;
                if (br->cond->v->type == CONSTANT) {
                    // replace branch with direct branch
                    // based on branch condition, which is a constant

                    auto cond_val = ((Constant *)br->cond->v)->value;
                    Basic_Block *direct_target = NULL;

                    if (cond_val) {
                        direct_target = br->true_target;
                    } else {
                        direct_target = br->false_target;
                    }

                    auto direct_br = new Instruction_Direct_Branch;
                    direct_br->target = direct_target;
                    direct_br->n = br->n;
                    direct_br->b = br->b;

                    bb->insts[bb->insts.len - 1] = direct_br;
                    br->drop_uses_of_operands();
                }
            }
        }
    }

    compute_CFG(f);

    // dfs all blocks, removing unreachable blocks
    mark_all_blocks_unvisited(f->blocks);
    Array<Basic_Block *> cleaned_blocks;
    Array<Basic_Block *> stack;

    stack.push(f->start_block);

    while (!stack.empty()) {
        auto bb = stack.back();
        stack.pop();

        if (bb_visited(bb))
            continue;
        mark_visited(bb);

        cleaned_blocks.push(bb);

        if (bb == f->exit_block)
            continue;

        assert(bb->insts.len != 0);
        auto last = bb->insts[bb->insts.len - 1];

        if (auto jmp = last->as<Instruction_Direct_Branch>()) {
            stack.push(jmp->target);
        } else if (auto br = last->as<Instruction_Branch>()) {
            stack.push(br->true_target);
            stack.push(br->false_target);
        } else if (auto ret = last->as<Instruction_Return>()) {
            stack.push(f->exit_block);
        } else {
            assert(false);
        }
    }

    // delete insturctions in dead blocks
    for (auto bb : f->blocks) {
        if (!bb_visited(bb)) {
            for (auto v : bb->insts) {
                v->drop_uses_of_operands();
            }
        }
    }

    // update phi operands and sources
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (v->type == PHI || v->type == MEMORY_PHI) {
                auto phi = (Phi *)v;
                for (auto pred : bb->preds) {
                    // @FIXME: does not visiting a bb means the edge is dead?
                    if (!bb_visited(pred)) {
                        int p = phi->sources.find(pred);
                        phi->sources.remove(pred);
                        auto use = phi->operands[p];
                        phi->operands.remove(use);
                        use->remove();
                    }
                }
            }
        }
    }

    f->blocks = cleaned_blocks;
    for (int i = 0; i < f->blocks.len; i++) {
        f->blocks[i]->index_in_procedure = i;
    }
    stack.release();
    remove_trivial_phi(f);
    compute_CFG(f);

    /*

    // update phi function
    // and remove trivial ones
    for(auto bb : f->blocks) {
        bool32 sources_all_live = true;
        for(int i = 0; i < bb->insts.len; i++) {
            auto v = bb->insts[i];
            if (v->type == PHI) {
                auto phi = (Phi *) v;
                Array<Use *> new_operands;
                Array<Basic_Block *> new_sources;

                for(int i = 0; i < phi->operands.len; i++) {
                    auto src = bb->preds[i];
                    Flow_Edge edge(src, bb);
                    bool32 edge_dead = (f->edges.find(edge) == f->edges.end());
                    if (edge_dead) {
                        printf("edge B%d->B%d is dead\n",
    src->index_in_procedure, bb->index_in_procedure); sources_all_live = false;
                    } else {
                        new_operands.push(phi->operands[i]);
                        new_sources.push(phi->sources[i]);
                    }
                }

                // no need to look at the remaining phi functions
                // because all edges coming to bb is live
                if (sources_all_live) break;

                phi->operands = new_operands;
                phi->sources = new_sources;

                Value *optimized_value = try_remove_trivial_phi(phi);
                if (optimized_value != (Value *)phi) {
                    // phi gets optimized out, delete phi from block
                    // @TODO: performance, use a linked list?
                    bb->insts.remove(phi);
                    i--; // @FIXME
                }

            } else {
                break;
            }
        }
    }
    */
}

} // namespace SIMPLIFY_CFG

OPT_PASS(simplify_cfg) {
    printf(">>> Simplifying CFG...\n\n");
    run_on_every_function(IR, SIMPLIFY_CFG::simplify_on_single_function);
}
