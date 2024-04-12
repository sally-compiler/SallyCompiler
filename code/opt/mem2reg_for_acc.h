

void move_value(Value *v, Basic_Block *bb, bool to_first = false) {
    if (v->b == bb)
        return;
    // @FIXME: crap performance
    v->b->insts.remove(v);
    v->b = bb;
    if (to_first) {
        bb->insts.insert(0, v);
    } else {
        bb->insts.insert(bb->insts.len - 1, v);
    }
}

// recognize simple loop which
// 1) has only one control veriable, i
// 2) i starts at 0
// 3) ends at a loop invariant
// 4) stride is 1 // @TODO: this is not nescessary?
// returns the control variable
Phi *get_induction_variable(Loop *l) {

    // checking if has loop has break, or mutiple conditions
    // if (l->header->preds.len != 0) return NULL; // make sure no continues

    for (auto bb : l->body) {
        if (bb == l->header)
            continue;
        for (auto succ : bb->succs) {
            if (l->body.find(succ) == -1) {
                return NULL;
            }
        }
    }

    auto br =
        l->header->insts[l->header->insts.len - 1]->as<Instruction_Branch>();
    assert(br);
    auto cond = br->cond->v->as<Instruction_Binary>();
    if (!cond)
        return NULL;
    if (cond->op_type != BINARY_LESS_THAN)
        return NULL;
    if (l->body.find(cond->rhs->v->b) != -1)
        return NULL;

    if (cond->lhs->v->type != PHI)
        return NULL;
    Phi *ind = (Phi *)cond->lhs->v;

    if (ind->operands.len != 2)
        return NULL;
    auto o1 = ind->operands[0]->v->as<Constant>();
    if (!o1)
        return NULL;
    if (o1->value != 0)
        return NULL;

    auto o2 = ind->operands[1]->v->as<Instruction_Binary>();
    if (!o2)
        return NULL;
    if (o2->op_type != BINARY_ADD)
        return NULL;
    if (o2->lhs->v != ind)
        return NULL;
    auto stride = o2->rhs->v->as<Constant>();
    if (!stride)
        return NULL;
    if (stride->value != 1)
        return NULL;

    return ind;
}

void mem2reg_for_acc_on_function(Procedure_IR *f) {
    bool changed = false;
    for (auto l : f->loops) {
        if (auto ind = get_induction_variable(l)) {
            printf("found induction variable: v%d\n", ind->n);
            auto inc = ind->operands[1]->v;

            for (auto inner : f->loops) {
                if (inner == l)
                    continue;
                if (!inner->is_innermost)
                    continue;

                Loop *p = inner->parent;
                while (p && p != l) {
                    p = p->parent;
                }
                if (!p)
                    continue;

                bool missed = false;
                Memory_Read *load = NULL;
                Memory_Write *store = NULL;
                for (auto u = ind->use; u; u = u->next) {
                    if (inner->body.find(u->user->b) == -1)
                        continue;
                    if (u->user == inc)
                        continue;
                    if (auto l = u->user->as<Memory_Read>()) {
                        if (load) {
                            missed = true;
                            break;
                        }
                        load = l;
                    } else if (auto s = u->user->as<Memory_Write>()) {
                        if (store) {
                            missed = true;
                            break;
                        }
                        store = s;
                    } else {
                        missed = true;
                        break;
                    }
                }

                if (!load)
                    continue;
                if (!store)
                    continue;
                if (missed)
                    continue;
                /*
                if (load->b->loop == l)  continue;
                if (store->b->loop == l) continue;
                if (load->b->loop != store->b->loop) continue;
                */

                auto acc = store->value_to_write->v->as<Instruction_Binary>();
                if (!acc)
                    continue;
                if (acc->op_type != BINARY_ADD)
                    continue;
                if (acc->lhs->v != load)
                    continue;

                // mem2reg
                changed = true;
                assert(inner->header->preds.len == 2);
                auto preheader = inner->header->preds[0]; // @FIXME
                auto latch = inner->header->succs[1];     // @FIXME
                assert(inner->body.find(latch) == -1);

                auto initial_read = load;
                auto write_after = store;

                move_value(initial_read, preheader, true);
                move_value(write_after, latch, true);

                auto tmp = new Phi(inner->header);
                for (auto pred : inner->header->preds) {
                    tmp->sources.push(pred);
                }
                tmp->operands.set_len(2);
                tmp->operands[0] = new_use(initial_read, tmp);
                tmp->operands[1] = new_use(acc, tmp);
                insert(inner->header, tmp, true);

                acc->lhs->remove();
                acc->lhs = new_use(tmp, acc);

                write_after->value_to_write->remove();
                write_after->value_to_write = new_use(tmp, write_after);
            }
        }
    }

    if (changed) {
        renumber_values(f);
    }
}

void mem2reg_for_acc(Program_IR *prog) {
    compute_loops(prog);
    run_on_every_function(prog, mem2reg_for_acc_on_function);
}
