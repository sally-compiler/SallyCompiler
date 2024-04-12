
#include "opt.h"

// @NOTE: every pass is included as a header file into opt.cpp
// so using namespace to avoid naming confliction
namespace DCE {

void dce_function(Procedure_IR *proc_ir) {

    renumber_blocks(proc_ir);
    renumber_values(proc_ir);

    int NB = proc_ir->blocks.len; // number of basic blocks

    // Compute post dominators
    auto dominated_by = make_2d<bool>(NB, NB);

    // exit block is only post-dominated by itself
    for (int i = 0; i < NB - 1; i++)
        dominated_by[NB - 1][i] = 0;
    dominated_by[NB - 1][NB - 1] = 1;

    // Initialized to N for all other blocks
    for (int i = 0; i < NB - 1; i++)
        for (int j = 0; j < NB; j++)
            dominated_by[i][j] = 1;

    Array<bool> temp(NB);
    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = NB - 2; i >= 0; i--) {
            for (int j = 0; j < NB; j++)
                temp[j] = 1;

            for (auto bb : proc_ir->blocks[i]->succs) {
                int k = bb->index_in_procedure;
                for (int m = 0; m < NB; m++) {
                    temp[m] &= dominated_by[k][m];
                }
            }
            temp[i] = 1;

            bool same = true;
            for (int j = 0; j < NB; j++) {
                if (temp[j] != dominated_by[i][j]) {
                    same = false;
                }
                dominated_by[i][j] = temp[j];
            }

            if (!same)
                changed = true;
        }
    }

    /* to find dominance frontiers for a block b
       first find b's dominator set DOM
       second find those block whose parent is in S, and is not dominated by b
       */
    auto rdf = make_2d<int>(NB); // dominance frontiers for each block

    for (int i = 0; i < NB; i++) {
        Array<int> doms;
        for (int j = 0; j < NB; j++) {
            if (dominated_by[j][i]) {
                doms.push(j);
            }
        }

        for (int j = 0; j < NB; j++) {

            bool is_child_of_dom = false;
            for (auto bb : proc_ir->blocks[j]->succs) {
                for (int f = 0; f < doms.len; f++) {
                    if (bb->index_in_procedure == doms[f]) {
                        is_child_of_dom = true;
                        break;
                    }
                }
            }

            bool not_strictly_dominated_by_i =
                (i == j) || (!dominated_by[j][i]);
            if (is_child_of_dom && not_strictly_dominated_by_i) {
                rdf[i].push(j);
            }
        }
    }

    /*
    printf("Reverse Dominance Frontier: \n");
    for(int i = 0; i < NB; i++) {
        printf("B%d: {", i);
        for(int j = 0; j < rdf[i].len; j++) {
            printf("B%d ", rdf[i][j]);
        }
        printf("}\n");
    }
    */

    Array<Value *> live_values;

    Array<bool> live_marks(proc_ir->values_count);
    // for(auto &m : live_marks) m = false; // all dead initially
    for (int i = 0; i < live_marks.len; i++) {
        live_marks[i] = false;
    }

    for (auto BB : proc_ir->blocks) {
        for (auto I : BB->insts) {
            if (I->type == INST_RETURN) {
                live_values.push(I);
            }
            if (I->type == MEMORY_DEF) {
                live_values.push(I);
            }
            if (I->type == MEMORY_PHI) {
                live_values.push(I);
            }
            if (I->type == MEMORY_WRITE) {
                live_values.push(I);
            }
            if (I->type == FUNCTION_CALL) { // TODO: only mark function call
                                            // that has side effects live
                live_values.push(I);
            }
        }
    }

    while (!live_values.empty()) {

        auto v = live_values.back();
        live_values.pop();
        live_marks[v->n] = true;
        Array<Use *> ops = get_operands(v);
        for (auto op_use : ops) {
            if (!live_marks[op_use->v->n]) {
                live_values.push(op_use->v);
            }
        }

        // if v is a phi function
        // mark its predecessors ('s branch inst) as live
        // otherwise its predecessors might get eliminated,
        // and phi doesn't know which branch the value flows in
        if (v->type == PHI || v->type == MEMORY_PHI) {
            for (auto pred : v->b->preds) {
                assert(pred->insts.len != 0);
                auto last_inst = pred->insts[pred->insts.len - 1];
                if ((last_inst->type == INST_DIRECT_BRANCH ||
                     last_inst->type == INST_BRANCH) &&
                    !live_marks[last_inst->n]) {
                    live_values.push(last_inst);
                }
            }
        }

        if (v->type == CONSTANT)
            continue;

        for (int rdf_block : rdf[v->b->index_in_procedure]) {
            Basic_Block *b = proc_ir->blocks[rdf_block];
            assert(b->insts.len != 0);
            auto last_inst = b->insts[b->insts.len - 1];
            if (last_inst->type == INST_BRANCH && !live_marks[last_inst->n]) {
                live_values.push(last_inst);
            }
        }
    }

    live_values.release();

    printf(">>> Eliminating dead code...\n\n");
    for (int n = 0; n < live_marks.len; n++) {
        if (!live_marks[n]) {
            // @TODO: don't tell me direct branch is dead
            printf("v%d is dead!\n", n);
        } else {
            // printf("v%d is alive!\n", i);
        }
    }
    printf("\n");

    int values_count_after_cleaning = 0;

    for (auto BB : proc_ir->blocks) {
        Array<Value *> cleaned_values;
        for (auto I : BB->insts) {
            if (live_marks[I->n] || I->type == INST_DIRECT_BRANCH) {
                cleaned_values.push(I);
                I->n = values_count_after_cleaning;
                values_count_after_cleaning++;
            } else if (I->type == INST_BRANCH) {
                I->drop_uses_of_operands();
                // rewrite useless branch with a jump to
                // nearest marked postdominator
                int from = I->b->index_in_procedure;
                int to = NB - 1; // far most post dominator
                for (int i = 0; i < dominated_by[from].len; i++) {
                    if (dominated_by[from][i]) {
                        if (dominated_by[i][to] && i != from) {
                            to = i;
                        }
                    }
                }
                printf("redirect B%d to B%d\n", from, to);

                auto direct_br = new Instruction_Direct_Branch;
                direct_br->target = proc_ir->blocks[to];
                direct_br->n = values_count_after_cleaning++;
                direct_br->b = I->b;
                cleaned_values.push((Value *)direct_br); // @FIXME no numbering

                I->b->succs.release();
                I->b->succs.push(proc_ir->blocks[to]);
            } else {
                I->drop_uses_of_operands();
            }
        }

        // "Copy" cleaned values into basic blocks
        BB->insts.release();
        BB->insts = cleaned_values;
    }

    Array<Basic_Block *> live_blocks;
    Queue<Basic_Block *> live_queue; // @Release
    Array<bool> visited(proc_ir->blocks.len);
    for (int i = 0; i < visited.len; i++) {
        visited[i] = false;
    }

    live_queue.push(proc_ir->start_block);
    visited[proc_ir->start_block->index_in_procedure] = true;

    while (!live_queue.empty()) {
        auto bb = live_queue.front();
        live_queue.pop();
        live_blocks.push(bb);
        for (auto succ : bb->succs) {
            if (!visited[succ->index_in_procedure]) {
                visited[succ->index_in_procedure] = true;
                live_queue.push(succ);
            }
        }
    }

    if (live_blocks.find(proc_ir->start_block) == -1) {
        live_blocks.push(proc_ir->start_block);
    }

    if (live_blocks.find(proc_ir->exit_block) == -1) {
        live_blocks.push(proc_ir->exit_block);
    }

    for (int i = 0; i < live_blocks.len; i++) {
        live_blocks[i]->index_in_procedure = i;
    }

    /*
    printf("live blocks: ");
    for(int i = 0; i < live_blocks.len; i++) {
        printf("B%d ", live_blocks[i]->index_in_procedure);
    }
    printf("\n\n");
    */

    // "Copy" cleaned blocks to IR
    proc_ir->blocks.release();
    proc_ir->blocks = live_blocks;

    proc_ir->values_count = values_count_after_cleaning;

    verify_bb(proc_ir);
    verify_phis(proc_ir);
    verify_opreands(proc_ir);

    compute_CFG(proc_ir);

    live_marks.release();
    visited.release();
}

} // namespace DCE

OPT_PASS(dead_code_elimination) {
    run_on_every_function(IR, DCE::dce_function);
}
