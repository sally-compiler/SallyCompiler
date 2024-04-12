#include "asm_passes.h"

void remove_identical_moves(Program_Asm *prog) {
    for (auto f : prog->functions) {
        for (auto mb : f->mbs) {
            for (auto I = mb->inst; I; I = I->next) {
                if (I->tag == MI_MOVE) {
                    auto mv = (MI_Move *)I;
                    if (mv->dst == mv->src) {
                        I->mark();
                    }
                }
            }
            mb->erase_marked_values();
        }
    }
}
