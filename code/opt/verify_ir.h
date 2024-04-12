
void verify_bb(Procedure_IR *f) {
    if (f->start_block->preds.len != 0)
        exit(60);
    if (f->exit_block->succs.len != 0)
        exit(61);
    for (auto bb : f->blocks) {
        if (bb->preds.len == 0 && bb != f->start_block) {
            exit(48);
        }
        if (bb->succs.len == 0 && bb != f->exit_block) {
            exit(47);
        }
        if (bb->preds.find(bb) != -1) {
            exit(46);
        }
        if (bb->index_in_procedure == -1) {
            exit(62);
        }

        // all bb should end with branch inst, except for exit block
        if (bb != f->exit_block) {
            auto last = bb->insts[bb->insts.len - 1];
            if (last->type != INST_BRANCH && last->type != INST_DIRECT_BRANCH &&
                last->type != INST_RETURN) {
                exit(63);
            }
        }
    }

    // disallow loops around non-conditioal bb
    for (auto bb : f->blocks) {
        if (bb->succs.len == 1) {
            auto succ = bb->succs[0];
            if (succ->succs.len == 1) {
                auto succ_succ = succ->succs[0];
                if (bb == succ_succ) {
                    if (!bb->is_condition_block && !succ->is_condition_block) {
                        exit(65);
                    }
                }
            }
        }
    }
}

void verify_phis(Procedure_IR *f) {

    // phis must come first
    for (auto bb : f->blocks) {
        bool past_first_non_phi = false;

        for (auto v : bb->insts) {
            if (v->type != PHI && v->type != MEMORY_PHI) {
                past_first_non_phi = true;
                continue;
            }
            if (past_first_non_phi &&
                (v->type == PHI || v->type == MEMORY_PHI)) {
                exit(64);
            }
        }
    }

    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (auto phi = v->as<Phi>()) {
                for (auto src : phi->sources) {
                    if (f->blocks.find(src) == -1) {
                        exit(49);
                    }
                }
            }
        }
    }

    int32 max_n = 0;
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (v->n > max_n) {
                max_n = v->n;
            }
        }
    }

    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (auto p = v->as<Phi>()) {
                for (auto opr : p->operands) {
                    if (opr->v->type == GLOBAL)
                        exit(202);
                    if (opr->v->type == ALLOCA)
                        exit(203);
                    if (opr->v->type == MEMORY_WRITE)
                        exit(204);
                    if (opr->v->b == NULL)
                        exit(201);
                    if (opr->v->n > max_n)
                        exit(200);
                    if (opr->v->n == -1)
                        exit(100);
                    if (opr->v->n < -1)
                        exit(99);
                }
            }
        }
    }
}

void verify_opreands(Procedure_IR *f) {

    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            for (auto use : get_operands(v)) {
                auto opr = use->v;
                if (opr->b == NULL)
                    exit(50);
                if (f->blocks.find(opr->b) == -1) {
                    if (opr->type == PHI)
                        exit(55);
                    else
                        exit(51);
                }
            }
        }
    }
}

void verify_use_list(Procedure_IR *f) {
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            for (auto u = v->use; u; u = u->next) {
                if (u->v != v)
                    assert(false);
                if (u->user->b == NULL)
                    exit(53);
                if (f->blocks.find(u->user->b) == -1)
                    exit(54);
            }
        }
    }
}

void verify_values(Procedure_IR *f) {
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (v->b != bb)
                exit(56);
        }
    }
}

void verify_function(Procedure_IR *f) {
    verify_bb(f);
    verify_phis(f);
    verify_opreands(f);
    verify_use_list(f);
    verify_values(f);
}

OPT_PASS(verify_ir) { run_on_every_function(IR, verify_function); }
