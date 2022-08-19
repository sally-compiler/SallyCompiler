
void dce_function(Procedure_IR *f) {
    Queue<Value *> worklist;
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            worklist.push(v);
        }
    }

    while (!worklist.empty()) {
        auto v = worklist.front();
        worklist.pop();
        if (!v->live)
            continue;
        if (v->type == MEMORY_WRITE || v->type == INST_RETURN ||
            v->type == MEMORY_DEF || v->type == MEMORY_PHI ||
            v->type == FUNCTION_CALL || v->type == INST_DIRECT_BRANCH ||
            v->type == INST_BRANCH)
            continue;

        if (v->use == NULL) {
            v->mark_remove_from_bb();

            for (auto u : get_operands(v)) {
                worklist.push(u->v);
            }

            v->drop_uses_of_operands();
        }
    }

    for (auto bb : f->blocks) {
        bb->remove_dead_values();
    }

    renumber_values(f);
}

void dce(Program_IR *prog) { run_on_every_function(prog, dce_function); }
