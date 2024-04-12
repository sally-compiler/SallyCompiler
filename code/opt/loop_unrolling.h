#include "opt.h"
/*return continue break
another while
*/
namespace UNROLL {
#define whole_unroll -1
#define twice 0
struct control_bound {
    Value *variable;
    Use *low = NULL;
    Use *high = NULL;
};
int32 curr_value_index = 1000;
int32 block_index;
Map<Value *, Value *> mapping;
Array<Use *> sign_variable;
typedef Pair<Value *, Value *> old_new;
Array<old_new *> dynamic_lates;

uint32 insert_into_basic_block(Basic_Block *basic_block, Value *v,
                               bool insert_to_first) {
    v->n = curr_value_index;
    v->b = basic_block;
    if (insert_to_first) {
        basic_block->insts.insert(0, v);
    } else {
        basic_block->insts.push(v);
    }
    return curr_value_index++;
}
Array<control_bound *> control;
int record_bound(Loop *native_loop) {
    int could_unroll = 0;
    auto bb = native_loop->header;
    while (bb->is_condition_block) {
        auto branch = (Instruction_Branch *)bb->insts.back();
        if (branch->cond->v->type == INST_BINARY) {
            auto curr = (Instruction_Binary *)branch->cond->v;
            switch (curr->op_type) {
            case BINARY_LESS_THAN: {
                if (curr->lhs->v->type == PHI && curr->lhs->v->b == bb) {
                    if (curr->rhs->v->type == CONSTANT) {
                        int index = curr->rhs->v->b->insts.find(curr->rhs->v);
                        auto ss = curr->rhs->v->b->insts;
                        while (ss[index + 1]->type == PHI) {
                            index++;
                        }
                        Constant *bound =
                            new Constant(((Constant *)curr->rhs->v)->value - 1);
                        bound->b = curr->rhs->v->b;
                        bound->n = curr_value_index++;
                        curr->rhs->v->b->insts.insert(index + 1, bound);
                        curr->rhs->remove();

                        curr->rhs = new_use(bound, curr);
                    } else {
                        Array<int> s;
                        int index = curr->rhs->v->b->insts.find(curr->rhs->v);
                        auto ss = curr->rhs->v->b->insts;
                        while (ss[index + 1]->type == PHI) {
                            index++;
                        }
                        Instruction_Binary *subtract = new Instruction_Binary;
                        subtract->b = curr->rhs->v->b;
                        subtract->n = curr_value_index++;
                        subtract->op_type = BINARY_SUBTRACT;
                        subtract->lhs = new_use(curr->rhs->v, subtract);
                        Constant *one = new Constant(1);
                        one->b = curr->rhs->v->b;
                        one->n = curr_value_index++;
                        curr->rhs->v->b->insts.insert(index + 1, one);
                        subtract->rhs = new_use(one, subtract);
                        curr->rhs->v->b->insts.insert(index + 2, subtract);
                        curr->rhs->remove();
                        curr->rhs = new_use(subtract, curr);
                    }
                } else if (curr->rhs->v->type == PHI && curr->rhs->v->b == bb) {
                    if (curr->lhs->v->type == CONSTANT) {
                        int index = curr->lhs->v->b->insts.find(curr->lhs->v);
                        auto ss = curr->lhs->v->b->insts;
                        while (ss[index + 1]->type == PHI) {
                            index++;
                        }
                        Constant *bound =
                            new Constant(((Constant *)curr->lhs->v)->value + 1);
                        bound->b = curr->lhs->v->b;
                        bound->n = curr_value_index++;
                        curr->lhs->v->b->insts.insert(index + 1, bound);
                        curr->lhs->remove();

                        curr->lhs = new_use(bound, curr);
                    } else {
                        int index = curr->lhs->v->b->insts.find(curr->lhs->v);
                        auto ss = curr->lhs->v->b->insts;
                        while (ss[index + 1]->type == PHI) {
                            index++;
                        }
                        Instruction_Binary *add = new Instruction_Binary;
                        add->op_type = BINARY_ADD;
                        add->b = curr->lhs->v->b;
                        add->n = curr_value_index++;
                        add->lhs = new_use(curr->lhs->v, add);
                        Constant *one = new Constant(1);
                        one->b = curr->lhs->v->b;
                        one->n = curr_value_index++;
                        curr->lhs->v->b->insts.insert(index + 1, one);
                        add->rhs = new_use(one, add);
                        curr->lhs->v->b->insts.insert(index + 2, add);
                        curr->lhs->remove();
                        curr->lhs = new_use(add, curr);
                    }
                } else {
                    could_unroll = 0;
                    break;
                } // else maybe is constant operate constant  .. Value  out of
                  // loop
            } // slide into <=,i don't konw it
            case BINARY_LESS_THAN_OR_EQUAL_TO: {
                curr->op_type =
                    BINARY_LESS_THAN_OR_EQUAL_TO; // because maybe slide  above
                if (curr->lhs->v->type == PHI &&
                    curr->lhs->v->b ==
                        bb) { // remove the third condition to below
                    if (((Phi *)curr->lhs->v)->operands.len == 2) {
                        control_bound *the_same_control = NULL;
                        for (auto yuansu : control) {
                            if (yuansu->variable == curr->lhs->v) {
                                the_same_control = yuansu;
                                break;
                            }
                        }
                        if (!the_same_control) {
                            auto elem = new control_bound;
                            elem->variable = curr->lhs->v;
                            elem->high = new_use(curr->rhs->v, curr);
                            auto val = ((Phi *)curr->lhs->v);
                            bool is_whole = false;
                            for (auto k : val->operands) {
                                if (k->v->type == CONSTANT &&
                                    native_loop->body.find(k->v->b) == -1 &&
                                    could_unroll != 2 &&
                                    curr->rhs->v->type == CONSTANT) {
                                    could_unroll = whole_unroll;
                                    is_whole = true;
                                }
                            }
                            if (!is_whole) {
                                could_unroll = twice; // no completely
                            }
                            control.push(elem);
                        } else {
                            Use *find_empty;
                            if (!the_same_control->high) {
                                the_same_control->high =
                                    new_use(curr->rhs->v, curr);
                            } else {
                                find_empty = the_same_control->high;
                                while (find_empty->next) {
                                    find_empty = find_empty->next;
                                }
                                find_empty = new_use(curr->rhs->v, curr);
                            }
                            auto val = ((Phi *)curr->lhs->v);
                            bool is_whole = false;
                            for (auto k : val->operands) {
                                if (k->v->type == CONSTANT &&
                                    native_loop->body.find(k->v->b) == -1 &&
                                    could_unroll != 2 &&
                                    curr->rhs->v->type ==
                                        CONSTANT) { /// asknkcs??????????
                                    could_unroll = whole_unroll;
                                    is_whole = true;
                                }
                            }
                            if (!is_whole) {
                                could_unroll = twice; // no completely
                            }
                        }
                    }
                } else if (curr->rhs->v->type == PHI && curr->rhs->v->b == bb) {
                    if (((Phi *)curr->rhs->v)->operands.len == 2) {
                        control_bound *the_same_control = NULL;
                        for (auto yuansu : control) {
                            if (yuansu->variable == curr->rhs->v) {
                                the_same_control = yuansu;
                                break;
                            }
                        }
                        if (!the_same_control) {
                            auto elem = new control_bound;
                            elem->variable = curr->lhs->v;
                            elem->low = new_use(curr->lhs->v, curr);
                            auto val = ((Phi *)curr->rhs->v);
                            bool is_whole = false;
                            for (auto k : val->operands) {
                                if (k->v->type == CONSTANT &&
                                    native_loop->body.find(k->v->b) == -1 &&
                                    could_unroll != 2 &&
                                    curr->lhs->v->type == CONSTANT) {
                                    could_unroll = whole_unroll;
                                    is_whole = true;
                                }
                            }
                            if (!is_whole) {
                                could_unroll = twice; // no completely
                            }
                            control.push(elem);
                        } else {
                            Use *find_empty;
                            if (!the_same_control->low) {
                                the_same_control->low =
                                    new_use(curr->lhs->v, curr);
                            } else {
                                find_empty = the_same_control->low;
                                while (find_empty->next) {
                                    find_empty = find_empty->next;
                                }
                                find_empty = new_use(curr->lhs->v, curr);
                            }
                            auto val = ((Phi *)curr->rhs->v);
                            bool is_whole = false;
                            for (auto k : val->operands) {
                                if (k->v->type == CONSTANT &&
                                    native_loop->body.find(k->v->b) == -1 &&
                                    could_unroll != 2 &&
                                    curr->lhs->v->type == CONSTANT) {
                                    could_unroll = whole_unroll;
                                    is_whole = true;
                                }
                            }
                            if (!is_whole) {
                                could_unroll = twice; // no completely
                            }
                        }
                    }
                } else {
                    could_unroll = 0;
                } // else maybe is constant operate constant  .. Value  out of
                  // loop
            } break;
            case BINARY_NOT_EQUAL_TO: {
                could_unroll = 0;
            } break;
            case BINARY_EQUAL_TO: {
                could_unroll = 0;
            } break;
            case BINARY_GREATER_THAN_OR_EQUAL_TO: {
                could_unroll = 0;
            } break;
            case BINARY_GREATER_THAN: {
                could_unroll = 0;
            } break;
            }
        }
        bb = ((Instruction_Branch *)bb->insts.back())->true_target;
    }
    return could_unroll;
}

void delete_edge(Basic_Block *from, Basic_Block *to, Procedure_IR *IR) {
    // Flow_Edge k(from,to);
    // IR->edges.erase(k);
    // from->succs.remove(to);
    // to->preds.remove(from);
}
Map<Basic_Block *, bool> visited;
Basic_Block *first_body;
bool dfs_find_first_block(Basic_Block *curr, Loop *nat_loop) {
    if ((!curr->is_condition_block) && (nat_loop->body.find(curr) != -1)) {
        first_body = curr;
        return true;
    }
    for (auto k : curr->succs) {
        bool found = dfs_find_first_block(k, nat_loop);
        if (found)
            return true;
    }
    return false;
}

void emit_mapping(Value *val, Loop *native_loop, bool another_while) {
    switch (val->type) {
    case PHI: {
        if (val->b != native_loop->header) {
            if (native_loop->body.find(val->b) == -1) {
                mapping[val] = val;
            } else {
                // do nothing ???
            }
            break;
        }
        if (another_while) {
            break;
        }
        auto p = ((Phi *)val);
        for (auto us : p->operands) {
            if (native_loop->body.find(us->v->b) != -1) {
                mapping[val] = us->v;
                printf("v%d --> v%d\n", val->n, us->v->n);
                if (sign_variable.find(us) == -1) {
                    sign_variable.push(us);
                    auto s = new old_new;
                    s->first = us->v;
                    s->second = NULL;
                    dynamic_lates.push(s);
                }
                /*else {
                        mapping[val] = val;
                }*/
            }
        }
    } break;
    default: {
        if (native_loop->body.find(val->b) == -1)
            mapping[val] = val;
    }
    }
}
/*below function no use*/
bool loop_variable(Value *val, Value *equal) {
    Use *curr = val->use;
    bool is_variable = false;
    while (curr) {
        if (curr->user == equal) {
            return true;
        }
        is_variable = loop_variable(curr->user, val);
        if (is_variable) {
            break;
        }
    }
    return is_variable;
}

Basic_Block *exit_block;
Map<Basic_Block *, Basic_Block *> map_block;
typedef Pair<Value *, Value *> standar_and_new;
Array<standar_and_new *> unseal_phi;
void mandatory_fix(Value *k) {
    if (mapping[k] == NULL) {
        mapping[k] = k;
    }
}
Basic_Block *dfs_copy(Basic_Block *standar, Loop *nat_loop, Procedure_IR *IR,
                      bool another_while = false) {

    Basic_Block *block = new Basic_Block;
    block->is_condition_block = standar->is_condition_block;
    block->is_loop_header = standar->is_loop_header;

    nat_loop->body.push(block);

    map_block[standar] = block;
    block->index_in_procedure = block_index++;
    IR->blocks.push(block);
    if (standar == nat_loop->exit) {
        exit_block = block;
    }
    bool is_first = true;
    Value *vv = NULL;
    Value *t = NULL;
    for (auto val : standar->insts) {
        t = val;
        switch (val->type) {
        case CONSTANT: {
            vv = (Value *)new Constant(((Constant *)val)->value);
            mapping[val] = vv;
        } break;
        case ARGUMENT: {
            vv = (Value *)new Argument;
            auto s = ((Argument *)val);
            ((Argument *)vv)->arg_index = s->arg_index;
            ((Argument *)vv)->decl = s->decl;
            mapping[val] = vv;
        } break;
        case GLOBAL: {
            vv = (Value *)new Global;
            ((Global *)vv)->name = ((Global *)val)->name;
            ((Global *)vv)->decl = ((Global *)val)->decl;
            mapping[val] = vv;
        } break;
        case PHI: {
            vv = (Value *)new Phi(block);
            auto s = ((Phi *)val);
            auto k = new standar_and_new;
            k->first = val;
            k->second = vv;
            unseal_phi.push(k);
            /*for (auto ope : s->operands) {
                    emit_mapping(ope->v, nat_loop);
                    ((Phi*)vv)->operands.push(new_use(mapping[ope->v], vv));
            }*/
            mapping[val] = vv;
        } break;
        case INST_BINARY: {
            vv = (Value *)new Instruction_Binary;
            emit_mapping(((Instruction_Binary *)val)->lhs->v, nat_loop,
                         another_while);
            emit_mapping(((Instruction_Binary *)val)->rhs->v, nat_loop,
                         another_while);
            ((Instruction_Binary *)vv)->lhs =
                new_use(mapping[((Instruction_Binary *)val)->lhs->v], vv);
            ((Instruction_Binary *)vv)->rhs =
                new_use(mapping[((Instruction_Binary *)val)->rhs->v], vv);
            ((Instruction_Binary *)vv)->op_type =
                ((Instruction_Binary *)val)->op_type;
            mapping[val] = vv;
        } break;
        case INST_UNARY: {
            vv = (Value *)new Instruction_Unary;
            auto s = ((Instruction_Unary *)val);
            emit_mapping(s->oprend->v, nat_loop, another_while);
            ((Instruction_Unary *)vv)->op_type =
                ((Instruction_Unary *)val)->op_type;
            ((Instruction_Unary *)vv)->oprend =
                new_use(mapping[s->oprend->v], vv);
            mapping[val] = vv;
        } break;
        case INST_RETURN: {
            vv = (Value *)new Instruction_Return;
            auto s = ((Instruction_Return *)val);

            if (s->return_value) {
                ((Instruction_Return *)vv)->return_value =
                    new_use(mapping[s->return_value->v], vv);
            }
        } break;
        case INST_BRANCH: {
            vv = (Value *)new Instruction_Branch;
            auto s = ((Instruction_Branch *)val);
            emit_mapping(((Instruction_Branch *)val)->cond->v, nat_loop,
                         another_while);
            mandatory_fix(s->cond->v);
            ((Instruction_Branch *)vv)->cond =
                new_use(mapping[((Instruction_Branch *)val)->cond->v], vv);
            ((Instruction_Branch *)vv)->true_target = map_block[s->true_target];
            if (nat_loop->body.find(s->false_target) == -1) {
                ((Instruction_Branch *)vv)->false_target = NULL;
            } else {
                ((Instruction_Branch *)vv)->false_target =
                    map_block[s->false_target];
            }
            mapping[val] = vv;
        } break;
        case INST_DIRECT_BRANCH: {
            vv = (Value *)new Instruction_Direct_Branch;
            vv->b = block;
            ((Instruction_Direct_Branch *)vv)->target = NULL;
            mapping[val] = vv;
        } break;
        case MEMORY_READ: {
            auto s = ((Memory_Read *)val);
            vv = (Value *)new Memory_Read;
            ((Memory_Read *)vv)->decl = s->decl;
            emit_mapping(s->base->v, nat_loop, another_while);
            if (s->offset) {
                emit_mapping(s->offset->v, nat_loop, another_while);
                ((Memory_Read *)vv)->offset =
                    new_use(mapping[s->offset->v], vv);
            } else {
                ((Memory_Read *)vv)->offset = NULL;
            }
            Value *watch = mapping[s->base->v];
            ((Memory_Read *)vv)->base = new_use(mapping[s->base->v], vv);
            if (s->mem_ver) {
                emit_mapping(s->mem_ver->v, nat_loop, another_while);
                ((Memory_Read *)vv)->mem_ver =
                    new_use(mapping[s->mem_ver->v], vv);
            } else {
                ((Memory_Read *)vv)->mem_ver = NULL;
            }

            mapping[val] = vv;
        } break;
        case MEMORY_WRITE: {
            auto s = ((Memory_Write *)val);
            vv = (Value *)new Memory_Write;
            ((Memory_Write *)vv)->decl = s->decl;
            if (s->value_to_write) {
                emit_mapping(s->value_to_write->v, nat_loop, another_while);
                ((Memory_Write *)vv)->value_to_write =
                    new_use(mapping[s->value_to_write->v], vv);
            } else {
                ((Memory_Write *)vv)->value_to_write = NULL;
            }
            emit_mapping(s->base->v, nat_loop, another_while);
            if (s->offset) {
                emit_mapping(s->offset->v, nat_loop, another_while);
                ((Memory_Write *)vv)->offset =
                    new_use(mapping[s->offset->v], vv);
            } else {
                ((Memory_Write *)vv)->offset = NULL;
            }
            ((Memory_Write *)vv)->base = new_use(mapping[s->base->v], vv);
            mapping[val] = vv;
        } break;
        case ALLOCA: {
            auto s = ((Alloca *)val);
            vv = (Value *)new Alloca;
            emit_mapping(s->size->v, nat_loop, another_while);
            ((Alloca *)vv)->size = new_use(mapping[s->size->v], vv);
            ((Alloca *)vv)->decl = s->decl;
            mapping[val] = vv;
        } break;
        case FUNCTION_CALL: {
            auto s = ((Function_Call *)val);
            vv = (Value *)new Function_Call;
            ((Function_Call *)vv)->name = s->name;
            ((Function_Call *)vv)->has_return_value = s->has_return_value;
            for (auto zhi : ((Function_Call *)val)->arguments) {
                emit_mapping(zhi->v, nat_loop, another_while);
                ((Function_Call *)vv)
                    ->arguments.push(new_use(mapping[zhi->v], vv));
            }
            mapping[val] = vv;
        } break;
        }
        insert_into_basic_block(block, (Value *)vv, is_first);
        vv->comment = val->comment;
        printf("v%d--->v%d\n", val->n, vv->n);
        for (int i = 0; i < dynamic_lates.len; i++) {
            if (val == dynamic_lates[i]->first) {

                if (!another_while) {
                    printf("change v%d :use[v%d  to v%d]\n",
                           sign_variable[i]->user->n, sign_variable[i]->v->n,
                           vv->n);
                    dynamic_lates[i]->second = vv;
                    break;
                }
            }
        }
        is_first = false;
    }
    Basic_Block *suc;
    visited[standar] = true;
    for (auto bb : standar->succs) {
        if (visited[bb] == false && nat_loop->body.find(bb) != -1) {
            suc = dfs_copy(bb, nat_loop, IR, another_while);
            // connect_CFG(block, suc);//maybe this is useless ,computcfg will
            // do
        }
    }
    if (vv != NULL && vv->type == INST_DIRECT_BRANCH) {
        ((Instruction_Direct_Branch *)vv)->target =
            map_block[((Instruction_Direct_Branch *)t)->target];
    }
    if (vv != NULL && vv->type == INST_BRANCH) {
        ((Instruction_Branch *)vv)->false_target =
            map_block[((Instruction_Branch *)t)->false_target];
        ((Instruction_Branch *)vv)->true_target =
            map_block[((Instruction_Branch *)t)->true_target];
    }

    return block;
}
uint32 seal_num = 1;
void seal_cfg(Loop *native_loop) {
    printf("begin to seal "
           "NO.%d=======================================================\n",
           seal_num++);

    for (auto p : unseal_phi) {
        printf("seal v%d=phi(", p->second->n);
        for (auto standar_us : ((Phi *)p->first)->operands) {
            if (p->first->b == native_loop->header) {
                if (native_loop->body.find(standar_us->v->b) == -1) {
                    ((Phi *)p->second)
                        ->operands.push(new_use(p->first, p->second));
                    // map_block[native_loop->header->preds[0]] =
                    // native_loop->header;
                } else {
                    auto ll = mapping[standar_us->v];
                    ((Phi *)p->second)
                        ->operands.push(
                            new_use(mapping[standar_us->v], p->second));
                }
            } else {
                if (native_loop->body.find(standar_us->v->b) == -1) {
                    ((Phi *)p->second)
                        ->operands.push(new_use(standar_us->v, p->second));
                    // map_block[native_loop->header->preds[0]] =
                    // native_loop->header;
                } else {
                    auto ll = mapping[standar_us->v];
                    ((Phi *)p->second)
                        ->operands.push(
                            new_use(mapping[standar_us->v], p->second));
                }
            }

            // printf("[v%d ]", mapping[pi->v]->n);
        }
        for (auto sou : ((Phi *)p->first)->sources) {
            if (p->first->b == native_loop->header &&
                sou == native_loop->header->preds[0]) {
                ((Phi *)p->second)->sources.push(native_loop->header);
            } else {
                ((Phi *)p->second)->sources.push(map_block[sou]);
                auto test = map_block[sou];
                printf("[b%d ]", map_block[sou]->index_in_procedure);
            }
        }
        printf(" )\n");
    }
    printf("end seal NO.%d===============================================\n",
           seal_num - 1);
}
Basic_Block *emit_new_cfg(Basic_Block *head, Loop *native_loop,
                          Procedure_IR *IR, bool another_while = false) {
    for (auto bb : native_loop->body) {
        visited[bb] = false;
    }
    if (!another_while) {
        for (auto val : native_loop->header->insts) {
            if (val->type == PHI) {
                auto p = ((Phi *)val);
                for (auto us : p->operands) {
                    if (native_loop->body.find(us->v->b) != -1) {
                        mapping[val] = us->v;
                        printf("v%d --> v%d\n", val->n, us->v->n);
                        if (sign_variable.find(us) == -1) {
                            sign_variable.push(us);
                            auto s = new old_new;
                            s->first = us->v;
                            s->second = NULL;
                            dynamic_lates.push(s);
                        }
                    }
                }
            }
        }
    }

    visited[native_loop->header] = true;
    Basic_Block *temp = dfs_copy(head, native_loop, IR, another_while);
    seal_cfg(native_loop);
    for (int i = 0; i < dynamic_lates.len; i++) {
        if (!another_while) {
            printf("change v%d :use[v%d  to v%d]\n", sign_variable[i]->user->n,
                   sign_variable[i]->v->n, dynamic_lates[i]->second->n);
            sign_variable[i]->v = dynamic_lates[i]->second;
        }
    }
    unseal_phi.release();
    return temp;
}
void remake_cfg(Procedure_IR *IR, Loop *native_loop, int unroll_number,
                int num) {
    if (unroll_number == 0) {
        /* do nothing*/
    } else if (unroll_number == whole_unroll) {
        if (native_loop->exit->insts.back()->type == INST_DIRECT_BRANCH) {
            Value *br = native_loop->exit->insts.back();
            native_loop->exit->insts.remove(br);
        }
        dfs_find_first_block(native_loop->header, native_loop);
        Basic_Block *b;
        Array<Basic_Block *> ex;
        Array<Basic_Block *> head;
        ex.push(native_loop->exit);
        for (int i = 0; i < num; i++) {
            b = emit_new_cfg(first_body, native_loop, IR);
            ex.push(exit_block);
            head.push(b);
        }
        int i = 0;
        for (; i < head.len; i++) {
            connect_CFG(ex[i], head[i], true);
        }

        for (auto usage : sign_variable) {
            mapping[usage->user] = usage->v;
        }
        auto t = ((Instruction_Branch *)native_loop->header->insts.back());
        connect_CFG(ex[i], t->false_target, true);

    } else if (unroll_number == 2) {
        delete_edge(native_loop->exit, native_loop->header, IR);
        Basic_Block *b3 =
            emit_new_cfg(native_loop->header, native_loop, IR, true);
        auto midd = ((Instruction_Branch *)native_loop->header->insts.back())
                        ->false_target;

        ((Instruction_Branch *)b3->insts.back())->false_target = midd;
        ((Instruction_Branch *)native_loop->header->insts.back())
            ->false_target = b3;

        Value *br = native_loop->exit->insts.back();
        native_loop->exit->insts.remove(br);

        dfs_find_first_block(native_loop->header, native_loop);
        Basic_Block *b1 = emit_new_cfg(first_body, native_loop, IR);

        Basic_Block *ex1 = exit_block;

        Basic_Block *b2 = emit_new_cfg(first_body, native_loop, IR);
        Basic_Block *ex2 = exit_block;
        // native_loop->exit->insts.push(br);

        for (int i = 0; i < sign_variable.len; i++) {
            int index = 0;
            for (index = 0;
                 index < ((Phi *)sign_variable[i]->user)->operands.len;
                 index++) {
                if (((Phi *)sign_variable[i]->user)->operands[index]->v ==
                    sign_variable[i]->v) {
                    Basic_Block *look =
                        ((Phi *)sign_variable[i]->user)->sources[index];
                    Basic_Block *watch =
                        map_block[((Phi *)sign_variable[i]->user)
                                      ->sources[index]];
                    ((Phi *)sign_variable[i]->user)->sources[index] = ex2;
                    break;
                }
            }
        }
        connect_CFG(native_loop->exit, b1, true);
        connect_CFG(ex1, b2, true);
        connect_CFG(ex2, native_loop->header,
                    true); // sasadascasaaxewasxxshuija;xscdjvhscjaox
        for (int i = 0; i < native_loop->header->insts.len; i++) {
            mapping[native_loop->header->insts[i]] = b3->insts[i];
        }

    } else if (unroll_number == 4) {

    } else {
        assert(false && "wo hai mei you zhun bei zhe me duo!");
    }
}
int compute_high_bound(Use *high) {
    Use *temp = high;
    int max_val = -0x7fffffff;
    while (temp) {
        int32 v = ((Constant *)temp->v)->value;
        max_val = v > max_val ? v : max_val;
        temp = temp->next;
    }
    return max_val;
}
int compute_low_bound(Use *low) {
    Use *temp = low;
    int min_val = -0x7fffffff;
    while (temp) {
        int v = ((Constant *)low->v)->value;
        min_val = v < min_val ? v : min_val;
        temp = temp->next;
    }
    return min_val;
}
void post_process(Procedure_IR *IR, Loop *native_loop, int we_can_unroll) {

    for (auto b : IR->blocks) {
        if (native_loop->body.find(b) == -1) {
            for (auto val : b->insts) {
                switch (val->type) {
                case PHI: {
                    auto phi = ((Phi *)val);
                    for (int i = 0; i < phi->operands.len; i++) {
                        auto us = phi->operands[i];
                        if (native_loop->body.find(us->v->b) != -1) {
                            us->remove();
                            phi->operands[i] = new_use(mapping[us->v], phi);
                        }
                    }

                } break;
                case INST_BINARY: {
                    auto binary = ((Instruction_Binary *)val);
                    if (native_loop->body.find(binary->lhs->v->b) != -1) {
                        binary->lhs->remove();
                        binary->lhs = new_use(mapping[binary->lhs->v], binary);
                    }
                    if (native_loop->body.find(binary->rhs->v->b) != -1) {
                        binary->rhs->remove();
                        binary->rhs = new_use(mapping[binary->rhs->v], binary);
                    }
                } break;
                case INST_UNARY: {
                    auto unary = ((Instruction_Unary *)val);
                    if (native_loop->body.find(unary->oprend->v->b) != -1) {
                        unary->oprend->remove();
                        unary->oprend =
                            new_use(mapping[unary->oprend->v], unary);
                    }
                } break;
                case FUNCTION_CALL: {
                    auto function = ((Function_Call *)val);
                    for (int i = 0; i < function->arguments.len; i++) {
                        auto us = function->arguments[i];
                        if (native_loop->body.find(us->v->b) != -1) {
                            us->remove();
                            function->arguments[i] =
                                new_use(mapping[us->v], function);
                        }
                    }
                } break;
                case MEMORY_READ: {
                    auto me_read = ((Memory_Read *)val);
                    if (me_read->base != NULL &&
                        native_loop->body.find(me_read->base->v->b) != -1) {
                        me_read->base->remove();
                        me_read->base =
                            new_use(mapping[me_read->base->v], me_read);
                    }
                    if (me_read->offset != NULL &&
                        native_loop->body.find(me_read->offset->v->b) != -1) {
                        me_read->offset->remove();
                        me_read->offset =
                            new_use(mapping[me_read->offset->v], me_read);
                    }
                    if (me_read->mem_ver != NULL &&
                        native_loop->body.find(me_read->mem_ver->v->b) != -1) {
                        me_read->mem_ver->remove();
                        me_read->mem_ver =
                            new_use(mapping[me_read->mem_ver->v], me_read);
                    }
                } break;
                case MEMORY_WRITE: {
                    auto me_write = ((Memory_Write *)val);
                    if (me_write->base != NULL &&
                        native_loop->body.find(me_write->base->v->b) != -1) {
                        me_write->base->remove();
                        me_write->base =
                            new_use(mapping[me_write->base->v], me_write);
                    }
                    if (me_write->offset != NULL &&
                        native_loop->body.find(me_write->offset->v->b) != -1) {
                        me_write->offset->remove();
                        me_write->offset =
                            new_use(mapping[me_write->offset->v], me_write);
                    }
                    if (me_write->value_to_write != NULL &&
                        native_loop->body.find(
                            me_write->value_to_write->v->b) != -1) {
                        me_write->value_to_write->remove();
                        me_write->value_to_write = new_use(
                            mapping[me_write->value_to_write->v], me_write);
                    }
                } break;
                case INST_RETURN: {
                    auto ret = ((Instruction_Return *)val);
                    if (ret->return_value != NULL &&
                        native_loop->body.find(ret->return_value->v->b) != -1) {
                        ret->return_value->remove();
                        ret->return_value =
                            new_use(mapping[ret->return_value->v], ret);
                    }
                } break;
                default: {

                } break;
                }
            }
        }
    }
    if (we_can_unroll == whole_unroll) {
        for (auto vv : native_loop->header->insts) {
            if (vv->type == PHI) {
                auto s = ((Phi *)vv);
                for (auto ins : s->operands) {
                    if (native_loop->body.find(ins->v->b) == -1) {
                        s->replace_all_uses_with(ins->v);
                    }
                }
            }
        }
        for (auto v : native_loop->header->insts) {
            v->drop_uses_of_operands();
        }
        native_loop->header->insts.release();
        auto k = new Instruction_Direct_Branch;
        k->target = first_body;
        insert_into_basic_block(native_loop->header, k, false);
    }
}
bool find_return_and_break(Loop *native_loop) {
    bool has_break_return = false;
    for (auto bb : native_loop->body) {
        if (bb != native_loop->header) {
            for (auto succ : bb->succs) {
                if (native_loop->body.find(succ) == -1) {
                    has_break_return = true;
                    return has_break_return;
                }
            }
        }
    }
    return has_break_return;
}
void unroll_function(Procedure_IR *cfg) {
    printf("unrolling_loop-----------------------------------------------------"
           "------------\n");
    for (auto native_loop : cfg->loops) {
        bool has_return_break = find_return_and_break(native_loop);
        if (native_loop->is_innermost && !has_return_break) {
            int we_can_unroll = record_bound(native_loop);
            if (we_can_unroll == 0) {
                continue;
            }
            for (int i = 0; i < control.len; i++) {
                // auto p = ((Phi*)control[i]->variable)->operands.front();
                for (auto p : ((Phi *)control[i]->variable)->operands) {
                    if (native_loop->body.find(p->v->b) !=
                        -1) { // find phi's value in the loop
                        if (p->v->type == INST_BINARY) {
                            auto temp = (Instruction_Binary *)p->v;
                            if (temp->lhs->v == control[i]->variable &&
                                temp->rhs->v->type ==
                                    CONSTANT) // occur i=i op k;k=constant
                            {
                                switch (temp->op_type) {
                                case BINARY_ADD: { // if op = +,i=i+2;
                                next:
                                    int n = 0;
                                    block_index = cfg->blocks.len;
                                    int times = 0;
                                    if (we_can_unroll == 2) {
                                        // need caliculate other high bound,no
                                        // just one
                                        remake_cfg(cfg, native_loop,
                                                   we_can_unroll, times);
                                        for (auto s :
                                             control[i]
                                                 ->high->v->b
                                                 ->insts) { // the upper bound
                                                            // of control
                                                            // variable
                                            if (s ==
                                                control[i]
                                                    ->high
                                                    ->v) { // find the
                                                           // declaration of
                                                           // upper bound
                                                auto cons = new Constant(
                                                    0); // bound - step * 3 +1
                                                cons->value =
                                                    ((Constant *)temp->rhs->v)
                                                            ->value *
                                                        3 -
                                                    1;
                                                cons->b =
                                                    control[i]->high->v->b;
                                                cons->n = curr_value_index++;
                                                control[i]
                                                    ->high->v->b->insts.insert(
                                                        n + 1, cons);
                                                auto sub =
                                                    new Instruction_Binary;
                                                sub->op_type = BINARY_SUBTRACT;
                                                sub->lhs =
                                                    new_use(((Value *)control[i]
                                                                 ->high->v),
                                                            sub); //

                                                sub->rhs =
                                                    new_use((Value *)cons, sub);
                                                control[i]
                                                    ->high->v->b->insts.insert(
                                                        n + 2, sub);
                                                sub->b = control[i]->high->v->b;
                                                sub->n = curr_value_index++;
                                                if (control[i]
                                                        ->high->user->type ==
                                                    INST_BINARY) { // if is i< m
                                                                   // or i <=
                                                                   // m(m ==
                                                                   // bound),find
                                                                   // the value
                                                    ((Instruction_Binary *)
                                                         control[i]
                                                             ->high->user)
                                                        ->rhs->v =
                                                        sub; // modify the bound
                                                             // to bound -step
                                                             // *3 +1
                                                    //((Instruction_Binary*)control[i]->high->user)->comment
                                                    //;//wait to modify comment
                                                }
                                            }
                                            n++;
                                        }
                                    } else if (we_can_unroll == whole_unroll) {
                                        int min =
                                            compute_low_bound(control[i]->low);
                                        int max = compute_high_bound(
                                            control[i]->high);
                                        for (auto s :
                                             ((Phi *)control[i]->variable)
                                                 ->operands) {
                                            if (native_loop->body.find(
                                                    s->v->b) == -1 &&
                                                s->v->type == CONSTANT) {
                                                Constant *t =
                                                    ((Constant *)s->v);
                                                if (t->value < min ||
                                                    t->value > max) {
                                                    times = 0;
                                                } else {
                                                    times = (max - t->value) /
                                                            (((Constant *)
                                                                  temp->rhs->v)
                                                                 ->value);
                                                }
                                            }
                                        }
                                        if (times > 60) {
                                            we_can_unroll = twice;
                                            goto next;
                                        }
                                        remake_cfg(cfg, native_loop,
                                                   we_can_unroll, times);
                                    } else {
                                        break;
                                    }
                                    bool is_add = true;
                                    post_process(cfg, native_loop,
                                                 we_can_unroll);
                                    compute_CFG(cfg);
                                } break;
                                default: {

                                } break;
                                }

                            } else if ((temp->lhs->v->type == CONSTANT &&
                                        temp->rhs->v == control[i]->variable)) {
                                switch (temp->op_type) {
                                case BINARY_ADD: { // if op = +,i=i+2;
                                next1:
                                    int n = 0;

                                    int times = 0;
                                    if (we_can_unroll == 2) {
                                        remake_cfg(cfg, native_loop,
                                                   we_can_unroll, times);
                                        // need caliculate other high bound,no
                                        // just one
                                        for (auto s :
                                             control[i]
                                                 ->high->v->b
                                                 ->insts) { // the upper bound
                                                            // of control
                                                            // variable
                                            if (s ==
                                                control[i]
                                                    ->high
                                                    ->v) { // find the
                                                           // declaration of
                                                           // upper bound
                                                auto cons = new Constant(
                                                    0); // bound - step * 3 +1
                                                cons->value =
                                                    ((Constant *)temp->rhs->v)
                                                            ->value *
                                                        3 -
                                                    1;
                                                cons->b =
                                                    control[i]->high->v->b;
                                                cons->n = curr_value_index++;
                                                control[i]
                                                    ->high->v->b->insts.insert(
                                                        n + 1, cons);
                                                auto sub =
                                                    new Instruction_Binary;
                                                sub->op_type = BINARY_SUBTRACT;
                                                sub->lhs =
                                                    new_use(((Value *)control[i]
                                                                 ->high->v),
                                                            sub); //

                                                sub->rhs =
                                                    new_use((Value *)cons, sub);
                                                control[i]
                                                    ->high->v->b->insts.insert(
                                                        n + 2, sub);
                                                sub->b = control[i]->high->v->b;
                                                sub->n = curr_value_index++;
                                                if (control[i]
                                                        ->high->user->type ==
                                                    INST_BINARY) { // if is i< m
                                                                   // or i <=
                                                                   // m(m ==
                                                                   // bound),find
                                                                   // the value
                                                    ((Instruction_Binary *)
                                                         control[i]
                                                             ->high->user)
                                                        ->rhs->v =
                                                        sub; // modify the bound
                                                             // to bound -step
                                                             // *3 +1
                                                    //((Instruction_Binary*)control[i]->high->user)->comment
                                                    //;//wait to modify comment
                                                }
                                            }
                                            n++;
                                        }

                                    } else if (we_can_unroll == whole_unroll) {
                                        int min =
                                            compute_low_bound(control[i]->low);
                                        int max = compute_high_bound(
                                            control[i]->high);
                                        for (auto s :
                                             ((Phi *)control[i]->variable)
                                                 ->operands) {
                                            if (native_loop->body.find(
                                                    s->v->b) == -1 &&
                                                s->v->type == CONSTANT) {
                                                Constant *t =
                                                    ((Constant *)s->v);
                                                if (t->value < min ||
                                                    t->value > max) {
                                                    times = 0;
                                                } else {
                                                    int mod =
                                                        (max - t->value + 1) %
                                                        (((Constant *)
                                                              temp->lhs->v)
                                                             ->value);
                                                    if (mod == 0) {
                                                        times =
                                                            (max - t->value +
                                                             1) /
                                                            (((Constant *)
                                                                  temp->lhs->v)
                                                                 ->value);
                                                    } else {
                                                        times =
                                                            ((max - t->value +
                                                              1) /
                                                             (((Constant *)
                                                                   temp->lhs->v)
                                                                  ->value)) +
                                                            1;
                                                    }
                                                }
                                            }
                                        }
                                        if (times > 60) {
                                            we_can_unroll = twice;
                                            goto next1;
                                        }
                                        remake_cfg(cfg, native_loop,
                                                   we_can_unroll, times);
                                    }
                                    block_index = cfg->blocks.len;
                                    bool is_add = true;

                                    post_process(cfg, native_loop,
                                                 we_can_unroll);
                                    compute_CFG(cfg);
                                } break;
                                default: {

                                } break;
                                }
                            }
                        }
                    }
                }
            }
            control.release();
            sign_variable.release();
            dynamic_lates.release();
        }
    }
    control.release();

    printf("ending "
           "unrolling_loop-----------------------------------------------------"
           "--------------\n");
}
} // namespace UNROLL

OPT_PASS(loop_unrolling) { run_on_every_function(IR, UNROLL::unroll_function); }
