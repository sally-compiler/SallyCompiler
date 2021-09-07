
/* This pass place machine blocks in depth-first order */

void place_function_blocks(Func_Asm *f) {

    Array<Machine_Block *> new_order;
    Array<Machine_Block *> stack;

    for(auto bb : f->mbs) bb->visited = false;

    stack.push(f->mbs[0]);

    while(!stack.empty()) {
        auto bb = stack.back(); stack.pop();

        if (bb->visited) continue;
        bb->visited = true;

        new_order.push(bb);

        if (bb->control_transfer_inst->tag == MI_RETURN) continue;

        assert(bb->control_transfer_inst->tag == MI_BRANCH);
        auto br = (MI_Branch *) bb->control_transfer_inst;

        if (br->cond == NO_CONDITION) {
            stack.push(br->true_target);
        } else {
            // group loop body together
            if ((br->true_target->belongs_to_loop != bb->belongs_to_loop) &&
                (br->true_target->loop_depth <= bb->loop_depth)) {
                stack.push(br->true_target);
                stack.push(br->false_target);
            } else {
                stack.push(br->false_target);
                stack.push(br->true_target);
            }
        }
    }


    f->mbs = new_order;
    for(int i = 0; i < f->mbs.len; i++) {
        f->mbs[i]->i = i;
    }
    stack.release();
}

void block_placement(Program_Asm *prog) {
    run_on_every_function(prog, place_function_blocks);
}
