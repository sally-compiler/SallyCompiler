
void remove_unused_functions(Program_IR *prog) {
    compute_callgraph(prog);
    Array<Procedure_IR *> used_functions;
    for (auto f : prog->procedures) {
        if (f->caller.len != 0 || strcmp(f->name, "main") == 0) {
            used_functions.push(f);
        } else {
            printf("removing unused function: %s\n", f->name);
        }
    }
    prog->procedures = used_functions;
}
