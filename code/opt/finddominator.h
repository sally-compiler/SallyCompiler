std::map<Basic_Block *, Array<Basic_Block *>>
find_dominator(Procedure_IR *Graph) {
    std::map<Basic_Block *, Array<Basic_Block *>> dom;
    auto r = Graph->start_block;
    dom[r].push(r);
    for (auto blo : Graph->blocks) {
        if (blo == r) {
            continue;
        }
        for (auto v : Graph->blocks) {
            dom[blo].push(v);
        }
    }
    bool changes_occur = true;
    while (changes_occur) {
        changes_occur = false;
        for (auto v : Graph->blocks) {

            if (v == r) {
                continue;
            }
            int32 length = dom[v].len;
            // calculate the intersection of all dom[pred]
            Array<Basic_Block *> intersection;
            ;
            if (v->preds.len > 0) {
                for (int i = 0; i < dom[v->preds[0]].len; i++) {
                    intersection.push(dom[v->preds[0]][i]);
                }
                for (auto k : intersection) {
                    for (auto pred : v->preds) {
                        if (dom[pred].find(k) == -1) {
                            intersection.remove(k);
                        }
                    }
                }
                if (intersection.find(v) == -1) {
                    intersection.push(v);
                }
                printf("intersection for dom[%d] = ", v->index_in_procedure);
                for (
                    auto k :
                    dom[v]) { // this compare the diffrernt may occur
                              // error!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    printf("%d ", k->index_in_procedure);
                    if (intersection.find(k) == -1) {
                        changes_occur = true;
                        // break;
                    }
                }
                printf("end intersection for dom[%d] \n",
                       v->index_in_procedure);
                // reassign
                dom[v].release();
                for (auto k : intersection) {
                    dom[v].push(k);
                }
            }
            // dom[v].release();
        }
    }
    return dom;
}

Map<Basic_Block *, int32> num;
Map<Basic_Block *, bool> visited;
int index = 0;
typedef Pair<Flow_Edge, Array<Basic_Block *>> Natural_Loop;
Array<Natural_Loop *> loops;
Array<Value *> invariant;
void DFS(Basic_Block *next, Map<Basic_Block *, Array<Basic_Block *>> dom,
         Procedure_IR *proc_ir, bool assign_or_findbackedge = true,
         bool clean = true) {
    if (clean) {
        index = 0;
        visited.clear();
    }
    if (assign_or_findbackedge)
        num[next] = index++;
    visited[next] = 1;
    for (auto block : next->succs) {
        if (!assign_or_findbackedge) {
            if (num[block] < num[next] && visited[block] == 1) {
                if (dom[next].find(block) != -1) {
                    auto nat_loop = new Natural_Loop;
                    nat_loop->first.first = next;
                    nat_loop->first.second = block;
                    nat_loop->second.push(block);

                    printf("found (header, exit) = (B%d, B%d)\n",
                           block->index_in_procedure, next->index_in_procedure);

                    // traverse path from the head to the tail,and push block
                    Basic_Block *curr = next;
                    Queue<Basic_Block *> live_queue;
                    Array<bool> visited(proc_ir->blocks.len);
                    for (int i = 0; i < visited.len; i++) {
                        visited[i] = false;
                    }
                    for (auto temp : curr->preds) {
                        if (temp != block)
                            live_queue.push(temp);
                    }
                    while (!live_queue.empty()) {
                        auto bb = live_queue.front();
                        live_queue.pop();
                        nat_loop->second.push(bb);
                        // printf("visiting B%d\n", bb->index_in_procedure);
                        for (auto pred : bb->preds) {
                            if (!visited[pred->index_in_procedure] &&
                                pred != block) {
                                visited[pred->index_in_procedure] = true;
                                live_queue.push(pred);
                            }
                        }
                    }
                    nat_loop->second.push(next);
                    loops.push(nat_loop);
                }
            }
        }
        if (visited[block] == 0)
            DFS(block, dom, proc_ir, assign_or_findbackedge, false);
    }
}

bool find_invariant_expression(Natural_Loop *native_loop) {
    bool change = false;
    for (auto block : native_loop->second) {
        for (auto val : block->insts) {
            if (val->invariant == true) {
                continue;
            }
            switch (val->type) {
            case CONSTANT: {
                if (native_loop->second.find(val->b) != -1) {
                    val->invariant = true;
                    invariant.push(val);
                }
                change = true;
            } break;
            case INST_BINARY: {
                auto k = (Instruction_Binary *)val;
                if (k->op_type >= 5) {
                    break;
                }
                if (k->rhs->v->type == CONSTANT ||
                    k->rhs->v->invariant == true) {
                    if (k->lhs->v->type == CONSTANT ||
                        k->lhs->v->invariant == true) {
                        val->invariant = true;
                        invariant.push(val);
                        change = true;
                        break;
                    }
                    if (native_loop->second.find(k->lhs->v->b) == -1) {
                        val->invariant = true;
                        invariant.push(val);
                        change = true;
                        break;
                    }
                } else if (k->lhs->v->type == CONSTANT ||
                           k->rhs->v->invariant == true) {
                    if (native_loop->second.find(k->rhs->v->b) == -1) {
                        val->invariant = true;
                        invariant.push(val);
                        change = true;
                        break;
                    }
                } else {
                    if (native_loop->second.find(k->rhs->v->b) == -1 &&
                        native_loop->second.find(k->lhs->v->b) == -1) {
                        val->invariant = true;
                        invariant.push(val);
                        change = true;
                        break;
                    }
                }
            } break;

            case INST_UNARY: {
                auto k = (Instruction_Unary *)val;
                if (k->oprend->v->type == CONSTANT ||
                    k->oprend->v->invariant == true) {
                    k->invariant = true;
                    change = true;
                    invariant.push(val);
                    break;
                } else if (native_loop->second.find(k->oprend->v->b) == -1) {
                    k->invariant = true;
                    invariant.push(val);
                    change = true;
                }
            } break;
            default: {
            }
            }
        }
    }
    return change;
}
void find_invariant(Natural_Loop *native_loop) { // this is a strange algorithm
    for (auto block : native_loop->second) {
        for (auto val : block->insts) {
            val->invariant = false;
        }
    }
    invariant.release();
    bool changed = true;
    while (changed) {
        changed = find_invariant_expression(native_loop);
    }
}
Array<Basic_Block *> get_dominate_exit(Natural_Loop *native_loop,
                                       Procedure_IR *proc_ir) {
    Array<Basic_Block *> exit_block;
    for (auto block : native_loop->second) {
        for (auto suc : block->succs) {
            if (native_loop->second.find(suc) == -1) {
                exit_block.push(block);
                break;
            }
        }
    }
    return exit_block;
}
void invariant_code_motion(Procedure_IR *proc_ir) {

    auto dom = find_dominator(proc_ir);
    for (auto bb : proc_ir->blocks) {
        printf("dom[%d] = ", bb->index_in_procedure);
        for (auto bbb : dom[bb]) {
            printf("%d ", bbb->index_in_procedure);
        }
        printf("\n");
    }
    DFS(proc_ir->start_block, dom, proc_ir);
    for (auto bb : proc_ir->blocks) {
        printf("block[%d].num = %d\n", bb->index_in_procedure, num[bb]);
    }
    DFS(proc_ir->start_block, dom, proc_ir, false);
    int index = 1;
    for (auto native_loop : loops) {
        Array<Basic_Block *> exit;
        printf("loop%d = ", index++);
        for (auto native : native_loop->second) {
            printf("block%d ", native->index_in_procedure);
        }
        printf("\n");
        exit = get_dominate_exit(native_loop, proc_ir);
        printf("exit block = ");
        for (auto bb : exit) {
            printf("block%d ", bb->index_in_procedure);
        }
        printf("\n");
        // Value* valu;
        find_invariant(native_loop);
        printf(" mark invariant value: ");
        for (auto vv : invariant) {
            printf("v%d ", vv->n);
        }

        printf("\n");

        for (auto val : invariant) {
            // condition 1 :statement s dominate all exits,except condition
            // block
            bool val_dominate_exits = true;
            for (auto exit_dominate : exit) {
                if (!exit_dominate->is_condition_block) {
                    if (dom[exit_dominate].find(val->b) == -1) {
                        val_dominate_exits = false;
                        break;
                    }
                }
            }
            // condition 2 :not assign to x,and I not sure my algorithm  is
            // right

            if (val_dominate_exits) {
                Use *curr = val->use;
                bool not_assign = true;

                while (curr) {
                    if (curr->user->type == PHI) {
                        if (curr->user->b->is_condition_block) {
                            not_assign = false;
                            break;
                        }
                        auto p = (Phi *)curr->user;
                        for (auto otherval : p->operands) {
                            if (native_loop->second.find(otherval->v->b)) {
                                not_assign = false;
                                break;
                            }
                        } // condition 3:all use in loop of x can be reach by
                          // the def x in s
                        if (not_assign) {
                            Use *c = p->use;
                            while (c) {
                                if (native_loop->second.find(c->user->b)) {
                                    not_assign = false;
                                    break;
                                }
                                c = c->next;
                            }
                        }
                    }
                    if (!not_assign) {
                        break;
                    }
                    curr = curr->next;
                }
                // motion code
                if (not_assign) {
                    printf("v%d in block%d\n ", val->n,
                           val->b->index_in_procedure);
                    native_loop->second[0]->preds[0]->insts.push(val);
                    printf(
                        "move v%d to block%d\n ", val->n,
                        native_loop->second[0]->preds[0]->index_in_procedure);
                    printf("remove v%d from block%d\n ", val->n,
                           val->b->index_in_procedure);
                    val->b->insts.remove(val);
                    printf(
                        "edit v%d->b to block%d\n ", val->n,
                        native_loop->second[0]->preds[0]->index_in_procedure);
                    val->b = native_loop->second[0]->preds[0];
                }
            }
        }
        auto t = native_loop->second[0]->preds[0]->insts[0];
        native_loop->second[0]->preds[0]->insts.remove(t);
        native_loop->second[0]->preds[0]->insts.push(t);
    }
}