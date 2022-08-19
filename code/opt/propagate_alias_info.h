
// This pass tries to progapate alias info into functions
// by copying callee function, and replacing the decl info of array arguments
// @TODO: recursive function
// This pass should be run after inlining

void propagate_alias_info(Program_IR *prog) {
    for (auto f : prog->procedures) {
        for (auto bb : f->blocks) {
            for (auto v : bb->insts) {
                auto call = v->as<Function_Call>();
                if (!call)
                    continue;
                if (call->f->is_builtin)
                    continue;

                Array<Value *> arr_args;
                for (int i = 0; i < call->arguments.len; i++) {
                    if (f->arguments[i]->decl->decl_type->tag == TYPE_ARRAY) {
                        arr_args.push(call->arguments[i]->v);
                    }
                }

                if (arr_args.len == 0)
                    continue;

                // see if one pair of args doesn't alias
                // if so, create a copy and propagte alias info
                bool all_alias = true;
                for (int i = 0; i < arr_args.len; i++) {
                    for (int j = i + 1; j < arr_args.len; j++) {
                        // @TODO: use GEP to work with partial arrays
                        auto decl1 = arr_args if (may_alias())
                    }
                }
            }
        }
    }
}
