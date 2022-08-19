
Map<Basic_Block *, Basic_Block *> map_block;
Map<Value *, Value *> mapping;
Map<Basic_Block *, bool> visited;

Array<Basic_Block *> *save_blocks(Procedure_IR *IR) {
    auto saveblock = new Array<Basic_Block *>;
    for (auto block : IR->blocks) {
        saveblock->push(block);
    }
    return saveblock;
}

Basic_Block *dfs_copy(Basic_Block *standar, Array<Basic_Block *> *new_ir,
                      Procedure_IR *IR) {

    Basic_Block *block = new Basic_Block;
    block->is_condition_block = standar->is_condition_block;
    block->is_loop_header = standar->is_loop_header;
    map_block[standar] = block;
    new_ir->push(block);

    // IR->blocks.push(block);//test correctness/validity

    bool is_first = true;
    Value *vv = NULL;
    Value *t = NULL;
    for (auto val : standar->insts) {
        t = val;
        switch (val->type) {

        case CONSTANT: {
            vv = (Value *)new Constant(((Constant *)val)->value);

        } break;

        case ARGUMENT: {
            auto s = ((Argument *)val);
            vv = (Value *)new Argument;
            ((Argument *)vv)->arg_index = s->arg_index;
            ((Argument *)vv)->decl = s->decl;
        } break;

        case GLOBAL: {
            auto s = (Global *)val;
            auto g = new Global;
            vv = g;
            g->name = s->name;
            g->decl = s->decl;
        } break;

        case MEMORY_DEF: {
            auto s = (Memory_Def *)val;
            auto d = new Memory_Def;
            vv = d;

            d->is_initial_version = s->is_initial_version;

            if (d->clobbers) {
                d->clobbers = new_use(mapping[s->clobbers->v], vv);
            }

            if (d->cause) {
                d->cause = new_use(mapping[s->cause->v], vv);
            }

        } break;

        case PHI: {
            vv = (Value *)new Phi(block);
        } break;

        case MEMORY_PHI: {
            vv = (Value *)new Memory_Phi(block);
        } break;

        case INST_BINARY: {
            vv = (Value *)new Instruction_Binary;
            ((Instruction_Binary *)vv)->lhs =
                new_use(mapping[((Instruction_Binary *)val)->lhs->v], vv);
            ((Instruction_Binary *)vv)->rhs =
                new_use(mapping[((Instruction_Binary *)val)->rhs->v], vv);
            ((Instruction_Binary *)vv)->op_type =
                ((Instruction_Binary *)val)->op_type;
        } break;

        case INST_UNARY: {
            vv = (Value *)new Instruction_Unary;
            auto s = ((Instruction_Unary *)val);
            ((Instruction_Unary *)vv)->op_type =
                ((Instruction_Unary *)val)->op_type;
            ((Instruction_Unary *)vv)->oprend =
                new_use(mapping[s->oprend->v], vv);
        } break;

        case INST_RETURN: {
            auto s = (Instruction_Return *)val;
            auto ret = new Instruction_Return;
            vv = ret;

            if (s->return_value) {
                ret->return_value = new_use(mapping[s->return_value->v], vv);
            }

        } break;

        case INST_BRANCH: {
            auto s = (Instruction_Branch *)val;
            auto br = new Instruction_Branch;
            vv = br;

            br->cond = new_use(mapping[s->cond->v], vv);

        } break;

        case INST_DIRECT_BRANCH: {
            vv = (Value *)new Instruction_Direct_Branch;
            vv->b = block;
            ((Instruction_Direct_Branch *)vv)->target = NULL;
        } break;

        case MEMORY_READ: {
            auto s = ((Memory_Read *)val);
            auto read = new Memory_Read;
            vv = read;

            read->decl = s->decl;
            read->base = new_use(mapping[s->base->v], vv);

            if (s->offset) {
                read->offset = new_use(mapping[s->offset->v], vv);
            }

            if (s->mem_ver) {
                read->mem_ver = new_use(mapping[s->mem_ver->v], vv);
            }

        } break;

        case MEMORY_WRITE: {
            auto s = ((Memory_Write *)val);
            auto write = new Memory_Write;
            vv = write;

            write->decl = s->decl;
            write->base = new_use(mapping[s->base->v], vv);

            if (s->offset) {
                write->offset = new_use(mapping[s->offset->v], vv);
            }

            if (s->value_to_write) {
                write->value_to_write =
                    new_use(mapping[s->value_to_write->v], vv);
            }

        } break;

        case ALLOCA: {
            auto s = ((Alloca *)val);
            vv = (Value *)new Alloca;
            ((Alloca *)vv)->size = new_use(mapping[s->size->v], vv);
            ((Alloca *)vv)->decl = s->decl;
        } break;

        case FUNCTION_CALL: {
            auto s = (Function_Call *)val;
            auto call = new Function_Call;
            vv = call;

            call->has_return_value = s->has_return_value;
            call->f = s->f;
            call->caller = s->caller;
            call->name = s->name;

            for (auto zhi : s->arguments) {
                call->arguments.push(new_use(mapping[zhi->v], vv));
            }

        } break;

        default:
            assert(false);
        }

        vv->type = val->type;
        insert(block, (Value *)vv, is_first);
        mapping[val] = vv;
        vv->comment = val->comment;
        is_first = false;
    }

    Basic_Block *suc;
    visited[standar] = true;
    for (auto bb : standar->succs) {
        if (visited[bb] == false) {
            suc = dfs_copy(bb, new_ir, IR);
            connect_CFG(block, suc);
            if (vv->type == INST_DIRECT_BRANCH) {
                ((Instruction_Direct_Branch *)vv)->target = suc;
            }
        } else if (visited[bb] == true) {
            connect_CFG(block, map_block[bb]);
            if (vv->type == INST_DIRECT_BRANCH) {
                ((Instruction_Direct_Branch *)vv)->target = map_block[bb];
            }
        }
    }

    if (vv && vv->type == INST_BRANCH) {
        ((Instruction_Branch *)vv)->false_target =
            map_block[((Instruction_Branch *)t)->false_target];
        ((Instruction_Branch *)vv)->true_target =
            map_block[((Instruction_Branch *)t)->true_target];
    }
    if (vv && vv->type == INST_DIRECT_BRANCH) {
        if (((Instruction_Direct_Branch *)vv)->target == NULL) {
            block->insts.remove(vv);
        }
    }
    return block;
}

void seal_copy_cfg(Array<Basic_Block *> &blocks) {
    for (auto block : blocks) {
        for (auto val : block->insts) {
            if (val->type == PHI || val->type == MEMORY_PHI) {
                Phi *copy_val = ((Phi *)mapping[val]);
                for (auto temp : ((Phi *)val)->operands) {
                    auto aa = mapping[temp->v];
                    copy_val->operands.push(new_use(aa, copy_val));
                }
                for (auto temp : ((Phi *)val)->sources) {
                    copy_val->sources.push(map_block[temp]);
                }
            }
        }
    }
}

Procedure_IR *copy_function(Procedure_IR *f) {

    map_block.clear();
    mapping.clear();
    visited.clear();

    auto new_f = new Procedure_IR;

    new_f->has_return_value = f->has_return_value;
    new_f->args_count = f->args_count;
    new_f->name = f->name;

    // @TODO: arguments

    dfs_copy(f->start_block, &new_f->blocks, f);

    new_f->start_block = map_block[f->start_block];
    new_f->exit_block = map_block[f->exit_block];

    seal_copy_cfg(f->blocks);

    for (int i = 0; i < f->args_count; i++) {
        auto new_arg = (Argument *)mapping[f->arguments[i]];
        new_f->arguments.push(new_arg);
    }

    for (auto bb : new_f->blocks) {
        for (auto v : bb->insts) {
            if (auto call = v->as<Function_Call>()) {
                if (call->f == f)
                    call->f = new_f;
                if (call->caller == f)
                    call->caller = new_f;
            }
        }
    }

    return new_f;
}
