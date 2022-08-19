
// this is undone...

#include <algorithm>

int32 n = 0;

bool need_alloc(MOperand m) { return (m.tag == REG) || (m.tag == VREG); }

void number_inst(Func_Asm *f) {

    n = 0;

    Array<Machine_Block *> stack;

    for (auto bb : f->mbs)
        bb->visited = false;

    stack.push(f->mbs[0]);

    while (!stack.empty()) {
        auto bb = stack.back();
        stack.pop();

        if (bb->visited)
            continue;
        bb->visited = true;

        // number the instructions within
        for (auto I = bb->inst; I; I = I->next) {
            I->n = n++;
        }

        if (bb->control_transfer_inst->tag == MI_RETURN)
            continue;

        assert(bb->control_transfer_inst->tag == MI_BRANCH);
        auto br = (MI_Branch *)bb->control_transfer_inst;

        if (br->cond == NO_CONDITION) {
            stack.push(br->true_target);
        } else {
            // group loop body together
            if ((br->true_target->belongs_to_loop != bb->belongs_to_loop) &&
                (br->true_target->loop_depth <= bb->loop_depth)) {
                stack.push(br->true_target);
                stack.push(br->false_target);
            } else {
                stack.push(br->false_target);
                stack.push(br->true_target);
            }
        }
    }
}

static Array<Set<MOperand>> live_in;
static Array<Set<MOperand>> live_out;

static Array<Set<MOperand>> use_set; // actually 'use without def' set
static Array<Set<MOperand>> def_set;

void analyse_liveness(Func_Asm *func_asm) {

    live_in.release();
    live_out.release();
    use_set.release();
    def_set.release();

    live_in.set_len(func_asm->mbs.len);
    live_out.set_len(func_asm->mbs.len);
    use_set.set_len(func_asm->mbs.len);
    def_set.set_len(func_asm->mbs.len);

    // get use and def sets for each machine block
    for (int i = 0; i < func_asm->mbs.len; i++) {
        for (auto I = func_asm->mbs[i]->inst; I; I = I->next) {
            Array<MOperand> defs = get_defs(I);
            Array<MOperand> uses = get_uses(I, func_asm->has_return_value);

            for (auto use : uses) {
                if (use.tag == VREG &&
                    def_set[i].find(use) == def_set[i].end()) {
                    use_set[i].insert(use);
                }
            }

            for (auto def : defs) {
                def_set[i].insert(def);
            }
        }
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = 0; i < func_asm->mbs.len; i++) {
            Set<MOperand> old_in = live_in[i];
            Set<MOperand> old_out = live_out[i];

            // live_in[i] = use_set[i] union (live_out[i]-def_set[i])
            live_in[i] = use_set[i];
            for (auto o : live_out[i]) {
                if (def_set[i].find(o) == def_set[i].end()) {
                    live_in[i].insert(o);
                }
            }

            live_out[i].clear();
            for (auto succ : func_asm->mbs[i]->succs) {
                for (auto succ_in : live_in[succ->i]) {
                    live_out[i].insert(succ_in);
                }
            }

            if (!changed) {
                if ((live_in[i] != old_in) || (live_out[i] != old_out)) {
                    changed = true;
                }
            }
        }
    }
}

Map<MOperand, int32> live_start;
Map<MOperand, int32> live_end;

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

void compute_live_intervals(Func_Asm *func_asm) {
    live_start.clear();
    live_end.clear();

    for (auto b : func_asm->mbs) {
        for (auto I = b->inst; I; I = I->next) {
            auto defs = get_defs(I);
            auto uses = get_uses(I, func_asm->has_return_value);

            for (auto d : defs) {
                if (!need_alloc(d))
                    continue;
                live_start[d] = 0x7fffffff;
                live_end[d] = 0;
            }

            for (auto u : uses) {
                if (!need_alloc(u))
                    continue;
                live_start[u] = 0x7fffffff;
                live_end[u] = 0;
            }
        }
    }

    for (auto b : func_asm->mbs) {
        auto live = live_out[b->i];
        for (auto I = func_asm->mbs[b->i]->last_inst; I; I = I->prev) {
            auto defs = get_defs(I);
            auto uses = get_uses(I, func_asm->has_return_value);

            for (auto def : defs) {
                if (need_alloc(def)) {
                    live_start[def] = MIN(live_start[def], I->n);
                    live.insert(def);
                }
            }

            Set<MOperand> new_live;
            for (auto u : uses) {
                if (need_alloc(u)) {
                    new_live.insert(u);
                }
            }
            for (auto l : live) {
                if (defs.find(l) == -1) {
                    new_live.insert(l);
                }
            }
            live = new_live;

            for (auto l : live) {
                if (!need_alloc(l))
                    continue;
                live_end[l] = MAX(live_end[l], I->n);
            }
        }
    }
}

struct Interval {
    MOperand m;
    int32 start;
    int32 end;
};

bool operator==(const Interval &x, const Interval &y) {
    return (x.m == y.m) && (x.start == y.end) && (x.end == y.end);
}

Array<Interval> live_intvs;
Set<uint8> reg_pool;
Map<MOperand, uint8> reg;
Map<MOperand, uint32> location;
Set<MOperand> spilled_vregs;
Array<Interval> active;
int32 stack_size = 0;

bool cmp_intv(const Interval &a, const Interval &b) {
    return a.start < b.start;
}

void expire_old_intervals(int i) {
    for (int j = 0; j < active.len; j++) {
        auto intj = active[j];
        if (intj.end > live_intvs[i].start) {
            return;
        }

        active.remove(intj);
        uint8 r = reg[intj.m];
        assert(r < REG_COUNT);
        reg_pool.insert(r);
    }
}

void spill_at_interval(int i) {
    auto spill = active.back();
    auto intv = live_intvs[i];
    if ((intv.m.tag != REG && spill.end > intv.end) || (intv.m.tag == REG)) {
        active.pop();
        uint8 r = reg[spill.m];
        reg[intv.m] = r;
        location[spill.m] = stack_size;
        spilled_vregs.insert(spill.m);
        printf("spilling ");
        print_operand(spill.m);
        printf("\n");

        int j = 0;
        for (; j < active.len; j++) {
            if (active[j].end >= intv.end) {
                break;
            }
        }

        active.insert(j, intv);
    } else {
        location[intv.m] = stack_size;
        spilled_vregs.insert(intv.m);
        printf("spilling ");
        print_operand(intv.m);
        printf("\n");
    }
    stack_size += 4;
}

void setup_stack(Func_Asm *f) {
    uint32 callee_size = 0, local_array_size = f->stack_size,
           spilled_size = stack_size, arg_size = 0;

    // save all callee save registers
    for (uint8 r = r0; r < REG_COUNT; r++) {
        if (is_callee_save(r) || r == lr) {
            callee_size += 4;
        }
    }

    // calculate arg size
    int max_arg_count = 0;
    for (auto mb : f->mbs) {
        for (auto I = mb->inst; I; I = I->next) {
            if (I->tag == MI_FUNC_CALL) {
                auto call = (MI_Func_Call *)I;
                if (call->arg_count > max_arg_count) {
                    max_arg_count = call->arg_count;
                }
            }
        }
    }

    if (max_arg_count > 4) {
        arg_size = (max_arg_count - 4) * 4;
    }

    // insert stores after defs, loads before uses
    for (auto mb : f->mbs) {
        for (auto I = mb->inst; I; I = I->next) {
            auto defs = get_defs(I);
            for (auto def : defs) {
                if (def.tag != VREG)
                    continue;
                if (spilled_vregs.find(def) == spilled_vregs.end())
                    continue;

                auto str = new MI_Store;
                str->mem_tag = MEM_SAVE_SPILL;
                str->reg = def;
                str->base = make_reg(sp);
                str->offset = make_imm(arg_size + location[def]);

                insert(str, I->next);
                replace_defs(I, def, str->reg);
            }

            auto uses = get_uses(I, f->has_return_value);
            uint8 r = r9; // using r9~r10 as temp

            for (auto use : uses) {
                if (use.tag != VREG)
                    continue;
                if (spilled_vregs.find(use) == spilled_vregs.end())
                    continue;

                assert(r == r9 || r == r10);
                auto ldr = new MI_Load;
                ldr->mem_tag = MEM_LOAD_SPILL;
                ldr->reg = make_reg(r++);
                ldr->base = make_reg(sp);
                ldr->offset = make_imm(arg_size + location[use]);

                insert(ldr, I);
                replace_uses(I, use, ldr->reg);
            }
        }
    }

    // insert prologue and epilogue
    // insert add/sub sp & push/pops
    auto push = new MI_Push;
    for (uint8 r = r0; r < REG_COUNT; r++) {
        if (is_callee_save(r) || r == lr) {
            push->operands.push(make_reg(r));
        }
    }

    auto total_size = local_array_size + spilled_size + arg_size;
    auto sub_sp = new MI_Binary(BINARY_SUBTRACT, make_reg(sp), make_reg(sp),
                                make_imm(total_size));
    auto add_sp = new MI_Binary(BINARY_ADD, make_reg(sp), make_reg(sp),
                                make_imm(total_size));

    if (total_size > 0) {
        insert(sub_sp, f->mbs[0]->inst);
    }
    insert(push, f->mbs[0]->inst);

    for (auto bb : f->mbs) {
        if (bb->last_inst && bb->last_inst->tag == MI_RETURN) {
            auto pop = new MI_Pop;
            for (uint8 r = r0; r < REG_COUNT; r++) {
                if (is_callee_save(r) || r == lr) {
                    pop->operands.push(make_reg(r));
                }
            }
            if (total_size > 0) {
                insert(add_sp, bb->last_inst);
            }
            insert(pop, bb->last_inst);
        }
    }

    // fixup local array base calc
    for (auto base : f->local_array_bases) {
        assert(base->tag == MI_BINARY);
        auto sub = (MI_Binary *)base;
        assert(sub->op == BINARY_SUBTRACT);
        assert(sub->lhs == make_reg(sp));
        assert(sub->rhs.tag == IMM && sub->rhs.value > 0);

        int32 offset_relative_to_sp =
            arg_size + spilled_size + local_array_size - sub->rhs.value;
        // @TODO replace with a mov directly
        // if offset relative to sp is 0

        sub->op = BINARY_ADD;
        sub->lhs = make_reg(sp);
        sub->rhs = make_imm(offset_relative_to_sp);
    }

    // fixup arg loading calc
    for (auto mb : f->mbs) {
        for (auto I = mb->inst; I; I = I->next) {
            if (I->tag != MI_LOAD)
                continue;
            auto ldr = (MI_Load *)I;
            if (!ldr)
                continue;
            if (ldr->mem_tag != MEM_LOAD_ARG)
                continue;

            int32 offset_value =
                ((ldr->offset.tag == SHAYEBUSHI) ? 0 : ldr->offset.value);
            uint32 ofst_rel_to_sp = arg_size + spilled_size + local_array_size +
                                    callee_size + offset_value;
            ldr->base.value = sp;
            ldr->offset = make_imm(ofst_rel_to_sp);
        }
    }

    // legalize imm, use r12 as temp
    for (auto mb : f->mbs) {
        for (auto I = mb->inst; I; I = I->next) {
            auto uses = get_uses(I, f->has_return_value);

            bool inst_need_legalize = false;
            MOperand use_of_imm;

            if (I->tag == MI_LOAD) {
                auto load = (MI_Load *)I;
                if (load->base.tag == IMM) {
                    goto done;
                }
            }

            if (I->tag == MI_LOAD || I->tag == MI_STORE) {
                auto load_or_store = (MI_Load *)I;
                if (load_or_store->offset.tag == IMM) {
                    use_of_imm = load_or_store->offset;
                    inst_need_legalize = !can_be_imm12(use_of_imm.value);
                    goto done;
                }
            }

            for (auto use : uses) {
                if (use.tag == IMM) {
                    use_of_imm = use;
                    inst_need_legalize = !can_be_imm_ror(use_of_imm.value);
                    goto done;
                }
            }

        done:;

            if (inst_need_legalize) {
                printf("%d cannot be imm!\n", use_of_imm.value);

                auto temp = make_reg(r12);
                MI_Load *ldr = emit_load_of_constant(temp, use_of_imm.value);
                insert(ldr, I);
                replace_uses(I, use_of_imm, temp);
            }
        }
    }
}

const int R = 9;

void allocate(Func_Asm *f) {

    stack_size = 0;
    active.release();
    reg.clear();
    location.clear();
    spilled_vregs.clear();
    reg_pool.clear();

    for (uint8 r = 0; r <= r8; r++) {
        reg_pool.insert(r);
    }

    for (uint8 r = 0; r < REG_COUNT; r++) {
        reg[make_reg(r)] = r;
    }

    for (int i = 0; i < live_intvs.len; i++) {
        auto intv = live_intvs[i];
        expire_old_intervals(i);

        if (active.len == R || reg_pool.empty()) {
            spill_at_interval(i);
        } else {

            if (intv.m.tag == REG) {
                /*
                if (reg_pool.find(intv.m.tag) == reg_pool.end()) {
                    MOperand occupied;
                    for(int j = 0; j < active.len; j++) {
                        if (reg[active[j].m] == intv.m.value) {
                            spill_at_interval(i);
                        }
                    }
                }
                */
            } else {
                uint8 r = *(reg_pool.begin());
                reg_pool.erase(r);
                reg[intv.m] = r;
            }

            int j = 0;
            for (; j < active.len; j++) {
                if (active[j].end >= intv.end) {
                    break;
                }
            }

            active.insert(j, intv);
        }
    }
}

void color_register(MOperand &m) {
    if (need_alloc(m)) {
        uint8 r = reg[m];
        assert(r < REG_COUNT);
        m.value = r;
        m.tag = REG;
    }

    /*
    if (m.tag == REG && is_callee_save(m.value)) {
        if (needs_save.find(m.value) == -1) {
            needs_save.push(m.value);
        }
    }
    */
}

void assign_reg(Func_Asm *f) {

    for (auto b : f->mbs) {
        for (auto I = b->inst; I; I = I->next) {
            switch (I->tag) {

            case MI_CLZ: {
                color_register(((MI_Clz *)I)->dst);
                color_register(((MI_Clz *)I)->operand);
            } break;

            case MI_MOVE: {
                color_register(((MI_Move *)I)->dst);
                color_register(((MI_Move *)I)->src);
            } break;

            case MI_BINARY: {
                color_register(((MI_Binary *)I)->dst);
                color_register(((MI_Binary *)I)->lhs);
                color_register(((MI_Binary *)I)->rhs);
            } break;

            case MI_COMPARE: {
                color_register(((MI_Compare *)I)->lhs);
                color_register(((MI_Compare *)I)->rhs);
            } break;

            case MI_LOAD:
            case MI_STORE: {
                color_register(((MI_Load *)I)->reg);
                color_register(((MI_Load *)I)->base);
                color_register(((MI_Load *)I)->offset);
            } break;

            case MI_RETURN:
            case MI_FUNC_CALL:
            case MI_PUSH: // we don't push/pop virtual regs, for now
            case MI_POP:
            case MI_BRANCH:
                break;

            default:
                exit(65);
                assert(false);
            }
        }
    }
}

void linear_scan_function(Func_Asm *f) {
    live_intvs.release();
    number_inst(f);
    analyse_liveness(f);
    compute_live_intervals(f);

    for (auto it = live_start.begin(); it != live_start.end(); it++) {

        // handle lr
        if (it->second == 0x7fffffff) {
            assert(it->first.tag == REG && it->first.value == lr);
            it->second = 0;
        }

        // insert interval into live_intvs, sorted by live_start from low to
        // high
        Interval intv{it->first, it->second, live_end[it->first]};
        live_intvs.push(intv);

        switch (it->first.tag) {
        case REG:
            printf("r%d", it->first.value);
            break;
        case VREG:
            printf("vr%d", it->first.value);
            break;
        }
        printf(" [%d, %d]\n", it->second, live_end[it->first]);
    }

    std::sort(live_intvs.begin(), live_intvs.end(), cmp_intv);

    allocate(f);
    for (auto it = reg.begin(); it != reg.end(); it++) {
        print_operand(it->first);
        printf(" <- ");
        print_operand(make_reg(it->second));
        printf("\n");
    }
    assign_reg(f);
    setup_stack(f);
}

ASM_PASS(linear_scan) {
    run_on_every_function(program_asm, linear_scan_function);
}
