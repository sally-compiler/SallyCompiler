
// turn
// x[0] = n;
// x[1] = x[0] * x[0]
// into
// x[0] = n;
// x[1] = n*n;
// Memory SSA must be run before this pass

void mem_copy_prop_on_function(Procedure_IR *f) {
    for (auto bb : f->blocks) {
        for (int i = 0; i < bb->insts.len; i++) {
            auto store = bb->insts[i]->as<Memory_Write>();
            if (!store)
                continue;
            auto def = bb->insts[i + 1]->as<Memory_Def>();
            assert(def);

            for (auto u = def->use; u; u = u->next) {
                auto load = u->user->as<Memory_Read>();
                if (!load)
                    continue; // could be memory phi, which we don't care
                if (load->base->v != store->base->v)
                    continue;
                if (load->offset == NULL ^ store->offset == NULL)
                    continue;
                if ((load->offset == NULL && store->offset == NULL) ||
                    (load->offset->v == store->offset->v)) {
                    load->replace_and_remove(store->value_to_write->v);
                }
            }
        }
    }
}

OPT_PASS(mem_copy_propagation) {
    run_on_every_function(IR, mem_copy_prop_on_function);
}
