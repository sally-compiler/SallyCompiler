#include "opt.h"

OPT_PASS(compute_callgraph) {

    for (auto f : IR->procedures) {
        f->callee.release();
        f->caller.release();
    }

    for (auto f : IR->procedures) {
        for (auto bb : f->blocks) {
            for (auto v : bb->insts) {
                auto call = v->as<Function_Call>();
                if (!call)
                    continue;

                int callee_index = find_func_index(IR, call->name);
                auto callee = IR->procedures[callee_index];
                call->f = callee;
                call->caller = f;

                if (f->callee.find(callee) == -1)
                    f->callee.push(callee);
                if (callee->caller.find(f) == -1)
                    callee->caller.push(f);
            }
        }
    }

    // does function has side effects?
    // naive solution:
    // if a function doesn't issue memory writes and reads and doesn't call
    // another function with side effects, it's then pure

    // all bulitin functions has side effects
    // other functions don't initially
    for (auto f : IR->procedures) {
        f->has_side_effect = f->is_builtin;
    }

    for (auto f : IR->procedures) {
        for (auto bb : f->blocks) {
            for (auto v : bb->insts) {
                if (v->type == MEMORY_WRITE || v->type == MEMORY_READ) {
                    f->has_side_effect = true;
                    goto done;
                }
            }
        }
    done:;
    }

    // propagate purity to calllers
    for (auto f : IR->procedures) {
        if (f->has_side_effect) {
            for (auto caller : f->caller) {
                caller->has_side_effect = true;
            }
        }
    }

    for (auto f : IR->procedures) {
        if (!f->has_side_effect && !f->is_builtin) {
            for (auto caller : IR->procedures) {
                bool removed_def = false;
                for (auto bb : caller->blocks) {
                    for (auto v : bb->insts) {
                        if (auto call = v->as<Function_Call>()) {
                            if (call->f == f) {
                                for (auto u = call->use; u;) {
                                    auto nxt = u->next;
                                    if (auto def = u->user->as<Memory_Def>()) {
                                        def->replace_and_remove(
                                            def->clobbers->v);
                                        removed_def = true;
                                    }
                                    u = nxt;
                                }
                            }
                        }
                    }
                }
                if (removed_def) {
                    remove_trivial_phi(caller);
                }
            }
        }
    }

    for (auto f : IR->procedures) {
        printf("%s:", f->name);
        if (f->has_side_effect) {
            printf(" has side effects\n");
        }
        /*
        if (is_function_recursive(f)) {
            printf("is recursive!");
            exit(1);
        }
        */
        printf("callees: ");
        for (auto callee : f->callee) {
            printf("%s ", callee->name);
        }
        printf("\n");
        printf("callers: ");
        for (auto caller : f->caller) {
            printf("%s ", caller->name);
        }
        printf("\n\n");
    }
}
