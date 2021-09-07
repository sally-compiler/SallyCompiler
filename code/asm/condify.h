
// turn branch into conditional executions

bool can_condify(Machine_Block *bb) {
    int i = 0;
    for(auto I=bb->inst; I != bb->control_transfer_inst; I=I->next) {
        i++; 
        if (i > 6) return false;
        if (I->tag == MI_FUNC_CALL) return false;
        if (I->tag == MI_PUSH) return false;
        if (I->tag == MI_POP) return false;
        if (I->tag == MI_COMPARE) return false;
    }
    return true;
}

void condify(Program_Asm *prog) {
    for(auto f : prog->functions) {
        for(auto bb : f->mbs) {
            if (bb->last_inst->tag != MI_BRANCH) continue;
            auto br = (MI_Branch *) bb->last_inst;
            if (br->cond == NO_CONDITION) continue;

            // see if one of the branches is empty

            if (br->true_target->preds.len == 1 &&
                br->false_target->preds.len == 1) continue;

            auto if_end = br->false_target;
            auto then_bb = br->true_target;
            if (br->true_target->preds.len == 2) {
                if_end = br->true_target;
                then_bb = br->false_target;
            }

            if (if_end->preds.len != 2)      continue;
            if (then_bb->preds.len != 1)     continue;
            if (then_bb->succs.len != 1)     continue;
            if (then_bb->succs[0] != if_end) continue;

            if (!can_condify(then_bb)) continue;

            if (if_end->i != bb->i+1 && if_end->i != bb->i+2)   continue;
            if (then_bb->i != bb->i+1 && then_bb->i != bb->i+2) continue;

            if (br->true_target != then_bb) {
                br->cond         = invert_branch_cond(br->cond);
                br->true_target  = then_bb;
                br->false_target = if_end;
            }

            if (bb->i + 1 != then_bb->i) {
                f->mbs[bb->i+1]    = then_bb;
                f->mbs[then_bb->i] = if_end;
                
                if_end->i = then_bb->i;
                then_bb->i = bb->i + 1;
            }

            assert(if_end->i == bb->i + 2);

            for(auto I=then_bb->inst; I != then_bb->control_transfer_inst; I=I->next) {
                I->cond = br->cond;
            }

            then_bb->condified = true;

        }
    }
}
