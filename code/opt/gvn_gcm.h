#include "opt.h"
#include <unordered_map>

namespace GVN_GCM {

/* GCM */

Basic_Block *find_LCA(Basic_Block *a, Basic_Block *b) {
    if (a == NULL)
        return b;
    while (a->dom_depth > b->dom_depth)
        a = a->imm_dom;
    while (b->dom_depth > a->dom_depth)
        b = b->imm_dom;
    while (a != b) {
        a = a->imm_dom;
        b = b->imm_dom;
    }
    return a;
}

void move_value(Value *v, Basic_Block *bb, bool to_first = false) {
    if (v->b == bb)
        return;
    // @FIXME
    v->b->insts.remove(v);
    v->b = bb;
    if (to_first) {
        bb->insts.insert(0, v);
    } else {
        bb->insts.insert(bb->insts.len - 1, v);
    }
}

// Don't move thses types of values(in other words, pinned)
// We only move constants, binary and unary insts, mem read/write, alloca and
// function calls
bool is_pinned(Value *v) {
    auto call = v->as<Function_Call>();
    auto arg = v->as<Argument>();
    auto load = v->as<Memory_Read>();

    return (v->type == PHI || v->type == INST_RETURN ||
            v->type == MEMORY_WRITE || v->type == MEMORY_DEF ||
            v->type == MEMORY_PHI || v->type == INST_BRANCH ||
            v->type == INST_DIRECT_BRANCH || (load && load->mem_ver == NULL) ||
            (call && (call->f == NULL || call->f->has_side_effect)) ||
            (arg && arg->arg_index <= 3));
}

void schedule_early(Procedure_IR *func_IR, Value *v) {
    if (v->visited)
        return;
    if (is_pinned(v))
        return;
    v->visited = true;
    Basic_Block *b = func_IR->start_block;

    for (auto opr : get_operands(v)) {
        schedule_early(func_IR, opr->v);
        if (b->dom_depth < opr->v->b->dom_depth) {
            b = opr->v->b;
        }
    }

    move_value(v, b);
    printf("v%d is scheduled early to B%d\n", v->n, v->b->index_in_procedure);
}

bool cmp_value_numbering(const Value *a, const Value *b) { return a->n < b->n; }

void schedule_late(Procedure_IR *func_IR, Value *v) {
    if (v->visited)
        return;
    if (is_pinned(v))
        return;
    v->visited = true;

    Basic_Block *lca = NULL;
    if (is_pinned(v)) {
        for (auto u = v->use; u; u = u->next) {
            schedule_late(func_IR, u->user);
        }
        return;
    } else {
        for (auto u = v->use; u; u = u->next) {
            schedule_late(func_IR, u->user);
            Basic_Block *use = u->user->b;
            if (u->user->type == PHI || u->user->type == MEMORY_PHI) {
                auto phi = (Phi *)u->user;
                auto i = phi->operands.find(u);
                use = phi->sources[i];
            }
            lca = find_LCA(lca, use);
        }
    }

    if (lca == NULL) {
        return;
    } // v is dead and isn't used anywhere

    //  @note:
    //  there's a limit of how late we can schedule Memory_Read.
    //  Memory_Read can only be scheduled late to a bb
    //  where the version of memory it's accessing is available!
    //  (which can be v->b itself, thus altering the original algorithm a little
    //  bit)
    if (auto read = v->as<Memory_Read>()) {
        while (lca != v->b) {
            if (!func_IR->trash[read->mem_ver->v][lca][read->decl]) {
                lca = lca->imm_dom;
            } else {
                break;
            }
        }
    }

    Basic_Block *best = lca;
    while (true) {
        if (lca->loop_depth < best->loop_depth) {
            best = lca;
        }
        if (lca == v->b)
            break;
        lca = lca->imm_dom;
    }

    move_value(v, best, v->type == MEMORY_READ);
    printf("v%d is scheduled late to B%d\n", v->n, best->index_in_procedure);
}

void schedule_value_within_bb(Value *v, Basic_Block *bb,
                              Array<Value *> &new_insts) {
    if (v->visited)
        return;
    if (v->b != bb)
        return;
    v->visited = true;
    for (auto opr : get_operands(v)) {
        // loop?
        schedule_value_within_bb(opr->v, bb, new_insts);
    }
    new_insts.push(v);
}

void schedule_late_load_within_bb(Basic_Block *bb) {
    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = 0; i < bb->insts.len; i++) {
            auto v = bb->insts[i];
            if (v->visited)
                continue;
            auto load = v->as<Memory_Read>();
            if (!load)
                continue;
            if (load->mem_ver == NULL)
                continue;
            load->visited = true;

            changed = true;

            Array<Value *> users;

            for (auto u = v->use; u; u = u->next) {
                if (u->user->b == load->b) {
                    users.push(u->user);
                }
            }

            for (int j = i + 1; j < bb->insts.len; j++) {
                auto v2 = bb->insts[j];
                if (users.find(v2) != -1) {
                    bb->insts.remove(load);
                    bb->insts.insert(j - 1, load);
                    goto done;
                }
                if (v2->type == FUNCTION_CALL || v2->type == MEMORY_WRITE) {
                    for (int k = j + 1; k < bb->insts.len; k++) {
                        auto def = bb->insts[k]->as<Memory_Def>();
                        if (!def)
                            break;

                        assert(def->cause->v == v2);
                        if (may_alias(load->decl, def->decl)) {
                            // can't schedule any later
                            // place the load before v2
                            bb->insts.remove(load);
                            bb->insts.insert(j - 1, load);
                            goto done;
                        }
                    }
                }
            }
            // otherwise, place load at the end
            v->b->insts.remove(v);
            bb->insts.insert(bb->insts.len - 1, v);
        done:;
        }
    }
}

void gcm(Procedure_IR *func_IR) {

    // DCE::eliminate_on_single_function(func_IR); // number the instruction
    compute_CFG(func_IR);

    mark_all_values_as_unvisted(func_IR);

    for (auto bb : func_IR->blocks) {
        // for(auto v : bb->insts) {
        for (int i = 0; i < bb->insts.len; i++) {
            auto v = bb->insts[i];
            if (is_pinned(
                    v)) { // pinned instructions remain in their original block
                v->visited = true;
                for (auto opr : get_operands(v)) {
                    schedule_early(func_IR, opr->v);
                }
            }
        }
    }

    mark_all_values_as_unvisted(func_IR);

    for (auto bb : func_IR->blocks) {
        for (auto v : bb->insts) {
            for (auto u = v->use; u; u = u->next) {
                schedule_late(func_IR, u->user);
            }
        }
    }

    mark_all_values_as_unvisted(func_IR);

    for (auto bb : func_IR->blocks) {
        Array<Value *> new_insts;
        for (auto v : bb->insts) {
            if (v->visited)
                continue;
            schedule_value_within_bb(v, bb, new_insts);
        }
        bb->insts.release();
        bb->insts = new_insts;
    }

    mark_all_values_as_unvisted(func_IR);

    for (auto bb : func_IR->blocks) {
        schedule_late_load_within_bb(bb);
    }
}

/* GVN */

typedef std::string VN;

// @CLEANUP
VN get_vn(Value *v) {
    std::stringstream ss;
    switch (v->type) {
    case INST_UNARY: {
        auto u = (Instruction_Unary *)v;
        ss << "U " << u->op_type << "v" << u->oprend->v->n;
    } break;

    case INST_BINARY: {
        auto b = (Instruction_Binary *)v;

        auto lhs_n = b->lhs->v->n, rhs_n = b->rhs->v->n;
        if (b->op_type == BINARY_ADD || b->op_type == BINARY_MULTIPLY) {
            if (lhs_n < rhs_n) {
                auto t = lhs_n;
                lhs_n = rhs_n;
                rhs_n = t;
            }
        }
        ss << "B "
           << "v" << lhs_n << "v" << b->op_type << "v" << rhs_n;

    } break;

    case CONSTANT: {
        auto c = (Constant *)v;
        ss << "C " << c->value;
    } break;

    case GLOBAL: {
        auto g = (Global *)v;
        ss << "G " << g->decl->glo_index;
    } break;

    case ARGUMENT: {
        auto a = (Argument *)v;
        ss << "A " << a->arg_index;
    } break;

    case FUNCTION_CALL: {
        auto call = (Function_Call *)v;
        ss << "F " << call->name << " ";

        for (auto arg : call->arguments) {
            ss << "v" << arg->v->n << " ";
        }

    } break;

    case MEMORY_READ: {
        auto read = (Memory_Read *)v;
        ss << "L ";
        ss << "v" << read->base->v->n << ", ";

        if (read->offset)
            ss << "v" << read->offset->v->n << ", ";

        // can only GVN memory loads with mem verion attached to it!
        ss << "v" << read->mem_ver->v->n;

    } break;

    default:
        assert(false);
    }
    return ss.str();
}

std::unordered_map<VN, Value *> vn_map;

int32 eval_value(Value *v) {
    if (auto bi = v->as<Instruction_Binary>()) {

        int32 l = eval_value(bi->lhs->v), r = eval_value(bi->rhs->v);

        uint32 ul = l;

        switch (bi->op_type) {
        case BINARY_ADD:
            return l + r;
        case BINARY_SUBTRACT:
            return l - r;
        case BINARY_MULTIPLY:
            return l * r;
        case BINARY_DIVIDE:
            return l / r;
        case BINARY_MOD:
            return l % r;
        case BINARY_LSL:
            return ul << r;
        case BINARY_LSR:
            return ul >> r;
        case BINARY_NOT_EQUAL_TO:
            return l != r;
        case BINARY_EQUAL_TO:
            return l == r;
        case BINARY_LESS_THAN_OR_EQUAL_TO:
            return l <= r;
        case BINARY_GREATER_THAN_OR_EQUAL_TO:
            return l >= r;
        case BINARY_LESS_THAN:
            return l < r;
        case BINARY_GREATER_THAN:
            return l > r;
        default:
            assert(false);
        }

    } else if (auto un = v->as<Instruction_Unary>()) {
        int32 opr = eval_value(un->oprend->v);
        switch (un->op_type) {
        case UNARY_NOT:
            return !opr;
        case UNARY_NEGATIVE:
            return -opr;
        case UNARY_POSITIVE:
            return +opr;
        default:
            assert(false);
        }
    } else if (auto constant = v->as<Constant>()) {
        return constant->value;
    }
    assert(false && "evaluating non-constant value");
    return 0;
}

void replace_if_already_computed(Procedure_IR *f, Value *v) {
    if (v->type == INST_BINARY || v->type == INST_UNARY ||
        v->type == FUNCTION_CALL || v->type == ARGUMENT ||
        v->type == MEMORY_READ || v->type == GLOBAL || v->type == CONSTANT) {

        VN vn = get_vn(v);
        auto it = vn_map.find(vn);

        if (it != vn_map.end()) {
            // v is a value that has been already computed
            v->replace_and_remove(it->second, true);
            v = it->second;
        } else {
            // v is a newly computed value
            vn_map[vn] = v;
        }

        if (auto arg_value = v->as<Argument>()) {
            f->arguments[arg_value->arg_index] = arg_value;
        }
    }
}

// @TODO: deal with --x, !!x
void number_value(Procedure_IR *f, Value *v) {

    if (v->visited)
        return;
    v->visited = true;

    if (v->type != PHI && v->type != INST_BINARY && v->type != ARGUMENT &&
        v->type != INST_UNARY && v->type != MEMORY_READ &&
        v->type != FUNCTION_CALL && v->type != GLOBAL && v->type != CONSTANT) {
        return;
    }

    // GVN pure functions only
    auto call = v->as<Function_Call>();
    if (call && (call->f == NULL || call->f->has_side_effect))
        return;

    // GVN those memory loads with memory version
    auto load = v->as<Memory_Read>();
    if (load && load->mem_ver == NULL)
        return;

    if (v->type == CONSTANT) {
        replace_if_already_computed(f, v);
        return;
    }

    // number each value in Reverse Post-Order
    for (auto opr : get_operands(v)) {
        number_value(f, opr->v);
    }

    // case 1, does v computes constant result?
    // 1) all opreands are constant
    // 2) x*0

    bool opr_const = true;
    for (auto opr : get_operands(v)) {
        if (opr->v->type != CONSTANT) {
            opr_const = false;
            break;
        }
    }
    if (opr_const && (v->type == INST_BINARY || v->type == INST_UNARY)) {
        int32 constant = eval_value(v);
        auto c = new Constant(constant);

        v->replace_inplace(c);

        return;
    }

    auto bi = v->as<Instruction_Binary>();
    if (bi && bi->op_type == BINARY_MULTIPLY) {
        auto lhs = bi->lhs->v->as<Constant>();
        auto rhs = bi->rhs->v->as<Constant>();
        if ((rhs && rhs->value == 0) || (lhs && lhs->value == 0)) {
            auto c = new Constant(0);

            bi->replace_inplace(c);

            return;
        }
    }

    // case 2, is v an algebraic identity?
    // x+0, x-0, x*1, x/1, phi(x, x), --x

    /*
    auto un = v->as<Instruction_Unary>();
    if (un && un->op_type == UNARY_NEGATIVE) {
        auto un_op = un->oprend->v->as<Instruction_Unary>();
        if (un_op && un_op->op_type == UNARY_NEGATIVE) {
            un_op->replace_and_remove(un_op->oprend->v, true);
            un->drop_uses_of_operands();
            un->mark_remove_from_bb();
            return;
        }
    }
    */

    if (bi) {
        auto lc = bi->lhs->v->as<Constant>();
        auto rc = bi->rhs->v->as<Constant>();

        if ((bi->op_type == BINARY_ADD || bi->op_type == BINARY_SUBTRACT) &&
            rc && rc->value == 0) {
            bi->replace_and_remove(bi->lhs->v, true);
            return;
        }

        if ((bi->op_type == BINARY_MULTIPLY || bi->op_type == BINARY_DIVIDE) &&
            rc && rc->value == 1) {
            bi->replace_and_remove(bi->lhs->v, true);
            return;
        }
    }

    if (v->type == PHI || v->type == MEMORY_PHI) {
        auto phi = (Phi *)v;
        Value *same = NULL;
        for (auto opr : phi->operands) {
            Value *op = opr->v;
            if (op == same || (op == (Value *)phi))
                continue;
            if (same != NULL)
                return; // phi is non-trivial, return directly
            same = op;
        }

        phi->replace_and_remove(same, true);
        return;
    }

    // case 3, is v already computed?
    // lookup in a hash table to see
    replace_if_already_computed(f, v);
}

void gvn(Procedure_IR *f) {
    if (f->args_count != 0) {
        f->arguments.release();
        for (int i = 0; i < f->args_count; i++) {
            f->arguments.push(NULL);
        }
    }
    mark_all_values_as_unvisted(f);
    vn_map.clear();

    // @TODO: is it okay to simply traverse like this?
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            number_value(f, v);
        }
        bb->remove_dead_values();
    }
}

void move_constant_to_rhs(Procedure_IR *f) {
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (auto bi = v->as<Instruction_Binary>()) {
                if (bi->lhs->v->type != CONSTANT)
                    continue;
                if (bi->rhs->v->type == CONSTANT)
                    continue;

                bool need_swap = false;

                // switch the operands directly if op is communative
                if (bi->op_type == BINARY_ADD ||
                    bi->op_type == BINARY_MULTIPLY ||
                    bi->op_type == BINARY_EQUAL_TO ||
                    bi->op_type == BINARY_NOT_EQUAL_TO) {
                    need_swap = true;
                } else if (bi->op_type == BINARY_LESS_THAN_OR_EQUAL_TO) {
                    // invert the condition for condition ops
                    bi->op_type = BINARY_GREATER_THAN;
                    need_swap = true;
                } else if (bi->op_type == BINARY_GREATER_THAN_OR_EQUAL_TO) {
                    bi->op_type = BINARY_LESS_THAN;
                    need_swap = true;
                } else if (bi->op_type == BINARY_LESS_THAN) {
                    bi->op_type = BINARY_GREATER_THAN_OR_EQUAL_TO;
                    need_swap = true;
                } else if (bi->op_type == BINARY_GREATER_THAN) {
                    bi->op_type = BINARY_LESS_THAN_OR_EQUAL_TO;
                    need_swap = true;
                }

                if (need_swap) {
                    auto t = bi->lhs;
                    bi->lhs = bi->rhs;
                    bi->rhs = t;
                }
            }
        }
    }
}

void gvn_gcm_on_single_function(Procedure_IR *f) {
    move_constant_to_rhs(f);

    DCE::dce_function(f);
    // dce_function(f);
    DOM::compute_dominator_on_single_function(f);
    LOOP::compute_function_loops(f);

    gvn(f);

    DCE::dce_function(f);
    // dce_function(f);

    /*
    // @FIXME: DCE and GVN might eliminate some function calls,
    // thus invalidating callgraph?
    LOOP::compute_function_loops(f);
    */

    gcm(f);

    for (auto bb : f->blocks)
        put_phis_to_start(bb);

    renumber_values(f);
}
} // namespace GVN_GCM

OPT_PASS(gvn_gcm) {
    run_on_every_function(IR, GVN_GCM::gvn_gcm_on_single_function);
}
