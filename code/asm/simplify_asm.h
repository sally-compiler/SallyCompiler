
#include "asm_passes.h"

namespace SIMPLIFY {

    Machine_Block *merge_block(Machine_Block *a, Machine_Block *b) {
        assert(a->last_inst && "merging to an empty block");
        auto a_last = a->control_transfer_inst->prev;

        delete a->control_transfer_inst;

        if (a_last) {
            a_last->next = b->inst;
        } else {
            a->inst = b->inst;
        }
        b->inst->prev = a_last;

        a->last_inst = b->last_inst;
        a->control_transfer_inst = b->control_transfer_inst;
        a->succs = b->succs;

        for(auto I=b->inst; I; I=I->next) {
            I->mb = a;
        }

        // change b's succssors' ref to a
        for(auto succ: b->succs) {
            for(int i = 0; i < succ->preds.len; i++) {
                if (succ->preds[i] == b) {
                    succ->preds[i] = a;
                }
            }
        }

        delete b;

        return a;
    }

    
    // 1. remove things like mov r0, r0
    // 2. simplify CFG
    void simplify_function_asm(Func_Asm *func_asm) {

        // find all pairs of block (b1, b2) where
        // b1 is the only succssor of b2 and 
        // b2 is the only pred of b1
        // we can combine them
        // Also eliminate empty blocks

        Array<bool> dead_blocks(func_asm->mbs.len);
        for(int i = 0; i < func_asm->mbs.len; i++) {
            bool empty = (func_asm->mbs[i]->inst == NULL);
            dead_blocks[i] = empty;
        }

        // eliminate direct-branch-only blocks
        for(int i = 0; i < func_asm->mbs.len; i++) {
            auto mb = func_asm->mbs[i];
            // @FIXME: don't eliminate start blcok!
            if (i != 0 && mb->inst != NULL && mb->inst == mb->last_inst && mb->inst->tag == MI_BRANCH) {
                dead_blocks[mb->i] = true;
                auto new_target = ((MI_Branch *)mb->inst)->true_target;
                auto succ = mb->succs[0];
                succ->preds.remove(mb);
                for(auto pred : mb->preds) {
                    assert(pred->last_inst->tag == MI_BRANCH);
                    auto pred_br = (MI_Branch *)pred->last_inst;
                    if (pred_br->true_target == mb) pred_br->true_target = new_target;
                    if (pred_br->false_target == mb) pred_br->false_target = new_target;

                    int p = pred->succs.find(mb); assert(p != -1);
                    pred->succs[p] = new_target;

                    succ->preds.push(pred);
                }


                delete mb;
            }
        }


        bool changed = true;
        while(changed) {
            changed = false;
            for(int j = func_asm->mbs.len-1; j >= 0; j--) {
                if (!dead_blocks[j]) {
                    auto mb1 = func_asm->mbs[j];
                    if (mb1->preds.len == 1) {
                        for(int i = func_asm->mbs.len-1; i >= 0; i--) {
                            if (j != i && !dead_blocks[i]) {
                                auto mb2 = func_asm->mbs[i];
                                bool is_parent_of_mb1 = (mb1->preds.find(mb2) != -1);
                                // @TODO: eliminate direct-branch-only blocks
                                if (is_parent_of_mb1 && (mb2->succs.len == 1)) {
                                    printf("merging B%d and B%d\n", i, j);
                                    merge_block(mb2, mb1);
                                    dead_blocks[j] = true;
                                    changed = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        int i, j;
        for(i = 0, j = 0; j < func_asm->mbs.len; j++) {
            if (!dead_blocks[j]) {
                func_asm->mbs[i] = func_asm->mbs[j];
                func_asm->mbs[i]->i = i;
                i++;
            }
        }
        func_asm->mbs.len = i;

    }

};

ASM_PASS(simplify_asm) {
    printf(">>> Simplifying the assembly...\n\n");
    run_on_every_function(program_asm, SIMPLIFY::simplify_function_asm);
    printf("\n");
    print_function_asm(program_asm->functions[0]);
    printf("\n");
}
