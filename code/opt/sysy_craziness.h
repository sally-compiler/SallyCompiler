
void sysy_craziness(Program_IR *prog) {
    for (auto f : prog->procedures) {
        for (auto bb : f->blocks) {
            for (int i = 0; i < bb->insts.len; i++) {
                auto v = bb->insts[i];
                if (auto call = v->as<Function_Call>()) {
                    if (0 == strcmp(call->name, "_sysy_starttime") ||
                        0 == strcmp(call->name, "_sysy_stoptime")) {
                        auto c = new Constant(0);
                        bb->insts.insert(i, c);
                        c->b = bb;
                        assert(call->arguments.len == 0);
                        call->arguments.push(new_use(c, call));
                        i++;
                    }
                }
            }
        }
        renumber_values(f);
    }
}
