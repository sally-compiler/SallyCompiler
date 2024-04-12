#include "opt.h"

// Sparse Conditional Constant Propagation
namespace SCCP {

enum State { UNDETERMINED = 0, CONST = 1, VARIABLE = 2 };

struct Lattice {
    State state;
    int32 value;

    // default constructor for Array<Lattice>
    Lattice() : state(UNDETERMINED), value(0) {}
    Lattice(State state, int32 value) : state(state), value(value) {}
};

static Lattice Undetermined(UNDETERMINED, 0);
static Lattice Variable(VARIABLE, 0);

// Lattice cells
Array<Lattice> cells;

Queue<Flow_Edge> flow_worklist;
Queue<Use *> ssa_worklist;
Set<Flow_Edge> executable_flag; // @cleanup

Lattice meet(Lattice l1, Lattice l2) {
    if (l1.state == UNDETERMINED) {
        return l2;
    }

    if (l2.state == UNDETERMINED) {
        return l1;
    }

    if (l1.state == VARIABLE || l2.state == VARIABLE) {
        return Variable;
    }

    if (l1.state == CONST && l2.state == CONST) {
        if (l1.value == l2.value) {
            return l1;
        } else {
            return Variable;
        }
    }

    assert(false);
    return Variable;
}

void visit_phi(Phi *phi) {
    auto bb = phi->b;
    Lattice new_cell = Undetermined;
    for (int i = 0; i < phi->operands.len; i++) {
        auto opr = phi->operands[i];
        auto from_bb = phi->b->preds[i];

        Flow_Edge edge{from_bb, bb};
        if (executable_flag.find(edge) != executable_flag.end()) {
            new_cell = meet(new_cell, cells[opr->v->n]);
        } else {
            new_cell = meet(new_cell, Undetermined);
        }
    }

    if (cells[phi->n].state != new_cell.state ||
        cells[phi->n].value != new_cell.value) {
        cells[phi->n] = new_cell;
        for (auto u = phi->use; u; u = u->next) {
            ssa_worklist.push(u);
        }
    }
}

void visit_expression(Value *v) {
    if (v->type == INST_DIRECT_BRANCH) {
        flow_worklist.emplace(v->b, v->b->succs[0]);
    } else if (v->type == INST_RETURN) {
        flow_worklist.emplace(v->b, v->b->succs[0]);
    } else if (v->type == INST_BRANCH) {
        auto br_inst = (Instruction_Branch *)v;
        auto cond_cell = cells[br_inst->cond->v->n];

        if (cond_cell.state == CONST) {
            if (cond_cell.value) {
                flow_worklist.emplace(v->b, br_inst->true_target);
            } else {
                flow_worklist.emplace(v->b, br_inst->false_target);
            }
        } else {
            for (auto succ : v->b->succs) {
                flow_worklist.emplace(v->b, succ);
            }
        }

    } else {
        Lattice old_cell = cells[v->n];
        Lattice new_cell = old_cell;
        switch (v->type) {
        case CONSTANT: {
            new_cell = {CONST, ((Constant *)v)->value};
        } break;

        // assume we don't know the values of arguments
        // and data from memory
        // can we?
        // also, don't know the global ref at runtime
        case GLOBAL:
        case ARGUMENT:
        case FUNCTION_CALL:
        case ALLOCA:
        case MEMORY_READ: {
            new_cell = Variable;
        } break;

        // values that aren't values
        case MEMORY_WRITE:
        case INST_RETURN:
            break;

        // already handled
        case PHI:
        case INST_BRANCH:
        case INST_DIRECT_BRANCH:
            assert(false);
            break;

        // the boss
        case INST_UNARY: {
            auto unary_inst = (Instruction_Unary *)v;
            Lattice opr_cell = cells[unary_inst->oprend->v->n];

            if (opr_cell.state == CONST) {
                int n;
                switch (unary_inst->op_type) {
                case UNARY_NOT: {
                    n = !opr_cell.value;
                } break;
                case UNARY_NEGATIVE: {
                    n = -opr_cell.value;
                } break;
                default:
                    assert(false);
                }
                new_cell = Lattice(CONST, n);
            } else if (opr_cell.state == VARIABLE) {
                new_cell = Variable;
            }

        } break;

        case INST_BINARY: {
            auto bi_inst = (Instruction_Binary *)v;
            Lattice lhs_cell = cells[bi_inst->lhs->v->n];
            Lattice rhs_cell = cells[bi_inst->rhs->v->n];

            bool32 both_const =
                (lhs_cell.state == CONST && rhs_cell.state == CONST);
            if (both_const) {

                int32 l = lhs_cell.value, r = rhs_cell.value,
                      n; // new value

                switch (bi_inst->op_type) {
                case BINARY_ADD: {
                    n = l + r;
                } break;
                case BINARY_SUBTRACT: {
                    n = l - r;
                } break;
                case BINARY_MULTIPLY: {
                    n = l * r;
                } break;
                case BINARY_DIVIDE: {
                    n = l / r;
                } break;
                case BINARY_MOD: {
                    n = l % r;
                } break;
                case BINARY_EQUAL_TO: {
                    n = l == r;
                } break;
                case BINARY_NOT_EQUAL_TO: {
                    n = l != r;
                } break;
                case BINARY_LESS_THAN_OR_EQUAL_TO: {
                    n = l <= r;
                } break;
                case BINARY_GREATER_THAN_OR_EQUAL_TO: {
                    n = l >= r;
                } break;
                case BINARY_LESS_THAN: {
                    n = l < r;
                } break;
                case BINARY_GREATER_THAN: {
                    n = l > r;
                } break;
                case BINARY_LOGICAL_OR: {
                    n = l || r;
                } break;
                case BINARY_LOGICAL_AND: {
                    n = l && r;
                } break;
                case BINARY_LSL: {
                    n = l << r;
                } break;
                case BINARY_LSR: {
                    n = l >> r;
                } break;

                default:
                    assert(false);
                }

                new_cell = Lattice(CONST, n);

            } else if (lhs_cell.state == CONST ||
                       rhs_cell.state == CONST) { // if one of them is constant

                if (rhs_cell.state == CONST) {
                    auto t = rhs_cell;
                    rhs_cell = lhs_cell;
                    lhs_cell = t;
                }

                int32 c = lhs_cell.value;

                if (bi_inst->op_type == BINARY_MULTIPLY && c == 0) {
                    new_cell = Lattice(CONST, 0);
                } else if (bi_inst->op_type == BINARY_LOGICAL_OR && c == 1) {
                    new_cell = Lattice(CONST, 1);
                } else if (bi_inst->op_type == BINARY_LOGICAL_AND && c == 0) {
                    new_cell = Lattice(CONST, 0);
                } else {
                    new_cell = meet(lhs_cell, rhs_cell);
                }
            } else {
                new_cell = meet(lhs_cell, rhs_cell);
            }

        } break;

        default:
            assert(false);
        }

        if (old_cell.state != new_cell.state ||
            old_cell.value != new_cell.value) {
            cells[v->n] = new_cell;
            for (auto u = v->use; u; u = u->next) {
                ssa_worklist.push(u);
            }
        }
    }
}

void prop_on_single_function(Procedure_IR *func_IR) {

    // flow_worklist.clear();
    // ssa_worklist.clear();
    renumber_blocks(func_IR);
    renumber_values(func_IR);
    executable_flag.clear();

    cells.set_len(func_IR->values_count);

    for (int n = 0; n < func_IR->values_count; n++) {
        cells[n].state = UNDETERMINED;
    }

    // initialize flow worklist to contain the edges exiting the start node
    for (auto v : func_IR->start_block->insts) {
        visit_expression(v);
    }

    while (!flow_worklist.empty() || !ssa_worklist.empty()) {

        if (!flow_worklist.empty()) {
            Flow_Edge flow_edge = flow_worklist.front();
            flow_worklist.pop();
            bool32 executed =
                (executable_flag.find(flow_edge) != executable_flag.end());

            if (!executed) {
                auto bb = flow_edge.second;
                bool visit_for_first_time = is_block_dead(func_IR, bb);
                executable_flag.insert(flow_edge);

                for (auto v : bb->insts) {
                    if (v->type == PHI) {
                        visit_phi((Phi *)v);
                    } else {
                        break;
                    }
                }

                // it's the first time this bb get visited
                if (visit_for_first_time) {
                    for (auto v : bb->insts) {
                        if (v->type != PHI) {
                            visit_expression(v);
                        }
                    }
                }
            }

        } else if (!ssa_worklist.empty()) {
            auto use = ssa_worklist.front();
            ssa_worklist.pop();

            if (use->user->type == PHI) {
                visit_phi((Phi *)(use->user));
            } else {
                auto bb = use->user->b;
                bool can_be_executed = false;
                for (auto pred : bb->preds) {
                    Flow_Edge edge{pred, bb};

                    can_be_executed =
                        (executable_flag.find(edge) != executable_flag.end());
                    if (can_be_executed) {
                        break;
                    }
                }

                if (can_be_executed) {
                    visit_expression(use->user);
                }
            }
        }
    }

    printf(">> SCCP finds: \n\n");
    /*
    for(int n = 0; n < cells.len; n++) {
        Lattice c = cells[n];

        const char *s;
        if (c.state == UNDETERMINED) s = "undetermined";
        if (c.state == CONST) s = "constant";
        if (c.state == VARIABLE) s = "variable";

        printf("v%d: %s", n, s);
        if (c.state == CONST) {
            printf("(%d)", c.value);
        }
        printf("\n");
    }
    printf("\n");
    */
    for (int i = 0; i < func_IR->blocks.len; i++) {
        if (is_block_dead(func_IR, func_IR->blocks[i])) {
            printf("B%d is dead\n", i);
        }
    }
    printf("\n");

    func_IR->edges = executable_flag;

    // replace values with constant
    for (auto bb : func_IR->blocks) {
        for (int i = 0; i < bb->insts.len; i++) {
            auto v = bb->insts[i];
            auto cell = cells[v->n];
            if (cell.state == CONST && v->type != CONSTANT) {
                printf("replacing v%d with constant %d\n", v->n, cell.value);
                auto new_v = new Constant(cell.value);
                v->replace_inplace(new_v);
            }
        }
    }

    printf("\n");

    // @TODO: eliminate dead branches
    // maybe with a cfg simplification pass
}

} // namespace SCCP

OPT_PASS(constant_prop) {
    run_on_every_function(IR, SCCP::prop_on_single_function);
}
