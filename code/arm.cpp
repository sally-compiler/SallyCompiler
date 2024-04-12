#include "arm.h"
#include "parser.h"
#include <cmath>

MOperand make_imm(int32 constant) { return MOperand(IMM, constant); }
MOperand make_reg(uint8 reg) { return MOperand(REG, reg); }
MOperand make_vreg(int32 vreg_index) { return MOperand(VREG, vreg_index); }

bool opt = false;

int is2power(int32 v) {
    if (v < 0)
        v = -v;
    int c = 0;
    if (v == 0)
        return 0;
    while (!(v & 1)) {
        c++;
        v >>= 1;
    }
    if (((v >> 1) & 0xFFFFFFFF) == 0)
        return c;
    return 0;
}

struct ms // for divide
{
    int m;
    int s;
};

ms magic(int d) {
    int p;
    unsigned ad, anc, delta, q1, r1, q2, r2, t;
    const unsigned two31 = 0x80000000;
    ms mag;

    ad = abs(d);
    t = two31 + ((unsigned)d >> 31);
    anc = t - 1 - t % ad;
    p = 31;
    q1 = two31 / anc;
    r1 = two31 - anc * q1;
    q2 = two31 / ad;
    r2 = two31 - ad * q2;
    do {
        p = p + 1;
        q1 *= 2;
        r1 *= 2;
        if (r1 >= anc) {
            q1++;
            r1 -= anc;
        }
        q2 *= 2;
        r2 *= 2;
        if (r2 >= ad) {
            q2++;
            r2 -= ad;
        }
        delta = ad - r2;
    } while (q1 < delta || (q1 == delta && r1 == 0));
    mag.m = q2 + 1;
    if (d < 0)
        mag.m = -mag.m;
    mag.s = p - 32;
    return mag;
}

struct mul_info {
    int s_val;
    int msb;
    int c_1;
};
mul_info can_optmul(int a) {
    mul_info info;
    info.s_val = 0;
    info.msb = 0;
    info.c_1 = 0;
    if (a == 0)
        return info;
    else {
        if (a < 0)
            a = -a;
        int get_c_1 = 1;
        int n = 30;
        while (n--) {
            if (a & get_c_1)
                info.c_1++;
            get_c_1 <<= 1;
        }
        int get_msb = 0x40000000;
        n = 31;
        while (n--) {
            if (a & get_msb) {
                info.msb = n;
                break;
            }
            get_msb >>= 1;
        }
        get_c_1 = 1;
        while (!(a & get_c_1)) {
            info.s_val++;
            get_c_1 <<= 1;
        }
        return info;
    }
}

void Machine_Block::erase_marked_values() {
    static Array<MI *> unmarked_values;
    unmarked_values.len = 0;

    for (MI *I = inst; I; I = I->next) {
        if (!I->marked) {
            unmarked_values.push(I);
        }
    }

    if (unmarked_values.len == 0) {
        inst = last_inst = control_transfer_inst = 0;
        succs = {};
    } else {
        inst = unmarked_values[0];
        last_inst = control_transfer_inst =
            unmarked_values[unmarked_values.len - 1];

        /*
        if (last_inst->prev && last_inst->prev->tag == MI_COMPARE) {
            control_transfer_inst = last_inst->prev;
        }
        */

        inst->prev = NULL;
        last_inst->next = NULL;

        unmarked_values.push(NULL);
        for (int i = 0; i < unmarked_values.len - 1; i++) {
            auto p = unmarked_values[i];
            auto n = unmarked_values[i + 1];
            p->next = n;
            if (n)
                n->prev = p;
        }
    }
}

// @FIXME: this is incorrect
MI *MI::erase_from_parent() {
    auto next_mi = next;
    if (prev == NULL && next == NULL) {
        mb->inst = mb->last_inst = NULL;
    } else if (prev == NULL && next != NULL) {
        mb->inst = next;
        next->prev = NULL;
    } else if (prev != NULL && next == NULL) {
        mb->last_inst = prev;
        prev->next = NULL;
    } else if (prev != NULL && next != NULL) {
        mb->last_inst = prev;
        prev->next = next;
        next->prev = prev;
    }

    // delete this;
    return next;
}

// insert the first MI before the second
void insert(MI *mi, MI *before) {
    if (before->prev == NULL) {
        before->mb->inst = mi;
    } else {
        before->prev->next = mi;
    }
    mi->mb = before->mb;
    mi->prev = before->prev;
    mi->next = before;
    before->prev = mi;
}

void push(MI *mi, Machine_Block *mb) {
    assert(mb->last_inst == NULL || (mb->last_inst->tag != MI_BRANCH &&
                                     mb->last_inst->tag != MI_RETURN));
    assert(mi->next == NULL);
    mi->mb = mb;
    if (mb->last_inst) {
        mb->last_inst->next = mi;
        mi->prev = mb->last_inst;
    } else {
        mb->inst = mi;
    }
    mb->last_inst = mi;
}

static Map<Value *, MOperand> value_map;
static int32 vreg_count = 0;

MI_Load *emit_load_of_constant(MOperand vreg, int32 constant) {
    auto ldr = new MI_Load;
    ldr->mem_tag = MEM_LOAD_FROM_LITERAL_POOL;
    ldr->reg = vreg;
    ldr->base = make_imm(constant);
    return ldr;
}

// @TODO: CLEANUP
MI_Move *emit_move(MOperand dst, MOperand src, Machine_Block *mb = NULL) {
    auto mv = new MI_Move(dst, src);
    /*
    if (src.tag == IMM && src.value < 0) {
        mv->src = make_imm(-src.value - 1);
        mv->neg = true;
    }
    */
    if (mb) {
        if (mb->control_transfer_inst) {
            insert(mv, mb->control_transfer_inst);
        } else {
            push(mv, mb);
        }
    }
    return mv;
}

bool can_be_imm_ror(int32 x) {
    uint32 v = x;
    for (int r = 0; r < 31; r += 2) {
        if ((v & 0xff) == v) {
            return true;
        }

        v = (v >> 2) | (v << 30);
    }
    return false;
}

// range of ldr/str imm offset
bool can_be_imm12(int32 x) { return (x >= -4095) && (x <= 4095); }

// Oh my...
Func_Asm *func_asm = NULL;

void emit_load_of_global_ref(Func_Asm *func_asm, Global *v, MOperand vreg,
                             Machine_Block *mb) {
    char *addr = new char[1024];
    addr = (char *)v->name;
    MOperand adr_global(ADR_GLOBAL, addr);
    auto ldr = new MI_Load;
    ldr->reg = vreg;
    ldr->base = adr_global;
    ldr->mem_tag = MEM_LOAD_GLOBAL_REF;
    push((MI *)ldr, mb);
}

void emit_load_of_later_arg(Argument *a, MOperand vreg, Machine_Block *mb) {
    assert(a->arg_index >= 4);
    auto offset_relative_to_fp = (a->arg_index - 4) * 4;
    auto ldr = new MI_Load;
    ldr->mem_tag = MEM_LOAD_ARG;
    ldr->reg = vreg;
    ldr->base = make_reg(sp);
    ldr->offset = make_imm(offset_relative_to_fp);

    if (mb->control_transfer_inst) {
        insert(ldr, mb->control_transfer_inst);
    } else {
        push(ldr, mb);
    }
}

// make SSA values into an operand. (Constant, or inst result)
// return the operand if already made
MOperand make_operand(Value *v, Machine_Block *mb, bool no_imm = false) {
    switch (v->type) {

    case CONSTANT: {
        int32 constant = ((Constant *)v)->value;
        auto imm = make_imm(constant);
        if (no_imm) {
            auto vreg = make_vreg(vreg_count++);
            // @FIXME: DO NOT PUSH DIRECTLY
            MI_Load *ldr = emit_load_of_constant(vreg, constant);
            if (mb) {
                if (mb->control_transfer_inst) {
                    insert(ldr, mb->control_transfer_inst);
                } else {
                    push(ldr, mb);
                }
            }
            // emit_move(vreg, imm, mb);
            return vreg;
        } else {
            return imm;
        }
    } break;

    case ARGUMENT: {
        auto arg_value = (Argument *)v;
        if (arg_value->arg_index >= 4 && !opt) {
            auto a = make_vreg(vreg_count++);
            emit_load_of_later_arg(arg_value, a, mb);
            return a;
        } else {
            auto opr = value_map.find(v);
            if (opr != value_map.end()) {
                return opr->second;
            }
            auto new_vreg = make_vreg(vreg_count++);
            value_map[v] = new_vreg;
            return new_vreg;
        }
    } break;

    case GLOBAL: {
        if (!opt) {
            auto glo_ref = (Global *)v;
            auto g = make_vreg(vreg_count++);
            emit_load_of_global_ref(func_asm, glo_ref, g, mb);
            return g;
        } else {
            auto opr = value_map.find(v);
            if (opr != value_map.end()) {
                return opr->second;
            }
            auto new_vreg = make_vreg(vreg_count++);
            value_map[v] = new_vreg;
            return new_vreg;
        }
    } break;

    default: {
        auto opr = value_map.find(v);
        if (opr != value_map.end()) {
            return opr->second;
        }
        auto new_vreg = make_vreg(vreg_count++);
        value_map[v] = new_vreg;
        return new_vreg;
    }
    }
}

void emit_binary_value_naive(Instruction_Binary *bi_I, Machine_Block *mb) {
    if (bi_I->op_type == BINARY_MOD) {
        auto bi_divide = new MI_Binary;
        bi_divide->op = BINARY_DIVIDE;
        bi_divide->lhs = make_operand(bi_I->lhs->v, mb, true);
        bi_divide->rhs = make_operand(bi_I->rhs->v, mb, true);
        bi_divide->dst = make_vreg(vreg_count++);
        push((MI *)bi_divide, mb);
        auto bi_mul = new MI_Binary;
        bi_mul->op = BINARY_MULTIPLY;
        bi_mul->lhs = bi_divide->dst;
        bi_mul->rhs = bi_divide->rhs;
        bi_mul->dst = make_vreg(vreg_count++);
        push((MI *)bi_mul, mb);
        auto bi_sub = new MI_Binary;
        bi_sub->op = BINARY_SUBTRACT;
        bi_sub->lhs = bi_divide->lhs;
        bi_sub->rhs = bi_mul->dst;
        bi_sub->dst = make_operand((Value *)bi_I, mb);
        push((MI *)bi_sub, mb);

    } else if (bi_I->op_type == BINARY_LOGICAL_AND) {
        // emit
        // mov vr0, r0
        // cmp vr0, #0
        // movne vr0, #1
        // mov vr1, r1
        // cmp vr1, #0
        // movne vr1, #1
        // and vr0, vr1, r1

        auto lhs = make_vreg(vreg_count++);
        auto rhs = make_vreg(vreg_count++);

        auto cmp_lhs_against_0 = new MI_Compare(lhs, make_imm(0));
        auto cmp_rhs_against_0 = new MI_Compare(rhs, make_imm(0));

        auto set_lhs_if_nonzero = new MI_Move(lhs, make_imm(1));
        auto set_rhs_if_nonzero = new MI_Move(rhs, make_imm(1));
        set_lhs_if_nonzero->cond = NOT_EQUAL;
        set_rhs_if_nonzero->cond = NOT_EQUAL;

        auto bitwise_and = new MI_Binary;
        bitwise_and->op = BINARY_BITWISE_AND;
        bitwise_and->dst = make_operand(bi_I, mb);
        bitwise_and->lhs = set_lhs_if_nonzero->dst;
        bitwise_and->rhs = set_rhs_if_nonzero->dst;

        emit_move(lhs, make_operand(bi_I->lhs->v, mb), mb);
        push(cmp_lhs_against_0, mb);
        push(set_lhs_if_nonzero, mb);
        emit_move(rhs, make_operand(bi_I->rhs->v, mb), mb);
        push(cmp_rhs_against_0, mb);
        push(set_rhs_if_nonzero, mb);
        push(bitwise_and, mb);

    } else if (bi_I->op_type == BINARY_LOGICAL_OR) {
        auto orr = new MI_Binary;
        orr->update_flags = true;
        orr->op = BINARY_BITWISE_OR;

        orr->dst = make_operand(bi_I, mb);
        orr->lhs = make_operand(bi_I->lhs->v, mb, true);
        orr->rhs = make_operand(bi_I->rhs->v, mb);

        auto set_1_if_nonzero = new MI_Move(orr->dst, make_imm(1));
        set_1_if_nonzero->cond = NOT_EQUAL;

        push(orr, mb);
        push(set_1_if_nonzero, mb);

    } else if (bi_I->op_type >= BINARY_NOT_EQUAL_TO &&
               bi_I->op_type <= BINARY_GREATER_THAN) {
        auto lhs = make_operand(bi_I->lhs->v, mb, true);
        auto rhs = make_operand(bi_I->rhs->v, mb);
        auto cmp = new MI_Compare(lhs, rhs);
        push(cmp, mb);

        auto result_vreg = make_operand(bi_I, mb);

        auto false_mv = new MI_Move(result_vreg, make_imm(0));
        auto true_mv = new MI_Move(result_vreg, make_imm(1));
        true_mv->cond = binary_op_to_branch_cond(bi_I->op_type);

        push(false_mv, mb);
        push(true_mv, mb);

    } else {

        auto lhs = make_operand(bi_I->lhs->v, mb, true);
        MOperand rhs;
        if (bi_I->op_type == BINARY_MULTIPLY || bi_I->op_type == BINARY_DIVIDE)
            rhs = make_operand(bi_I->rhs->v, mb,
                               /* no imm for mul and sdiv */ true);
        else
            rhs = make_operand(bi_I->rhs->v, mb);

        auto dst = make_operand((Value *)bi_I, mb);
        auto bi = new MI_Binary(bi_I->op_type, dst, lhs, rhs);
        push((MI *)bi, mb);
    }
}

void emit_binary_value(Instruction_Binary *bi_I, Machine_Block *mb) {
    if (!opt)
        return emit_binary_value_naive(bi_I, mb);
    assert(!is_relational_binary_op(bi_I->op_type));
    int p2 = 0;
    int is_constant = 0;
    int is_1 = 0;
    int is_n1 = 0;
    if (bi_I->rhs->v->type == CONSTANT) {
        if (((Constant *)(bi_I->rhs->v))->value == 1)
            is_1 = 1;
        else if (((Constant *)(bi_I->rhs->v))->value == -1)
            is_n1 = 1;
        else if (is2power(((Constant *)(bi_I->rhs->v))->value))
            p2 = is2power(((Constant *)(bi_I->rhs->v))->value);
        is_constant = 1;
    }
    if (bi_I->op_type == BINARY_MOD && (is_1 || is_n1)) {
        auto mov = new MI_Move(make_operand((Value *)bi_I, mb), make_imm(0));
        push((MI *)mov, mb);
    } else if (bi_I->op_type == BINARY_MOD && p2) {
        auto asr = new MI_Binary;
        asr->op = BINARY_ASR;
        asr->lhs = make_operand(bi_I->lhs->v, mb, true);
        asr->rhs = make_imm(31);
        asr->dst = make_vreg(vreg_count++);
        push((MI *)asr, mb);
        auto add = new MI_Binary;
        add->op = BINARY_ADD;
        add->lhs = asr->lhs;
        add->rhs = asr->dst;
        add->rhs.s_tag = LSR;
        add->rhs.s_value = 32 - p2;
        add->dst = make_vreg(vreg_count++);
        push((MI *)add, mb);
        auto bic = new MI_Binary;
        bic->op = BINARY_BIC;
        bic->lhs = add->dst;
        bic->rhs = make_imm((1 << p2) - 1);
        bic->dst = make_vreg(vreg_count++);
        push((MI *)bic, mb);
        auto sub = new MI_Binary;
        sub->op = BINARY_SUBTRACT;
        sub->lhs = asr->lhs;
        sub->rhs = bic->dst;
        sub->dst = make_operand((Value *)bi_I, mb);
        push((MI *)sub, mb);
    } else if (bi_I->op_type == BINARY_MOD && is_constant) {
        int32 dividend = ((Constant *)(bi_I->rhs->v))->value;
        assert(dividend < -1 || dividend > 1);
        ms magic_number;
        magic_number = magic(dividend);
        auto bi_smmul = new MI_Binary;
        bi_smmul->op = BINARY_SMMUL;
        bi_smmul->lhs = make_operand(bi_I->lhs->v, mb, true);
        bi_smmul->rhs = make_vreg(vreg_count++);
        bi_smmul->dst = make_vreg(vreg_count++);
        auto ldr = emit_load_of_constant(bi_smmul->rhs, magic_number.m);

        push(ldr, mb);
        push(bi_smmul, mb);

        MOperand smmul_next = bi_smmul->dst;
        if (dividend > 0 && magic_number.m < 0) {
            auto bi_add = new MI_Binary;
            bi_add->op = BINARY_ADD;
            bi_add->lhs = bi_smmul->dst;
            bi_add->rhs = bi_smmul->lhs;
            bi_add->dst = make_vreg(vreg_count++);
            push((MI *)bi_add, mb);
            smmul_next = bi_add->dst;
        }
        if (dividend < 0 && magic_number.m > 0) {
            auto sub = new MI_Binary;
            sub->op = BINARY_SUBTRACT;
            sub->lhs = bi_smmul->dst;
            sub->rhs = bi_smmul->lhs;
            sub->dst = make_vreg(vreg_count++);
            push((MI *)sub, mb);
            smmul_next = sub->dst;
        }
        MOperand before_add;
        before_add = smmul_next;
        if (magic_number.s > 0) {
            auto bi_asr = new MI_Binary;
            bi_asr->op = BINARY_ASR;
            bi_asr->lhs = smmul_next;
            bi_asr->rhs = make_imm(magic_number.s);
            bi_asr->dst = make_vreg(vreg_count++);
            push((MI *)bi_asr, mb);
            before_add = bi_asr->dst;
        }
        auto bi_add = new MI_Binary;
        bi_add->op = BINARY_ADD;
        bi_add->lhs = before_add;
        bi_add->rhs = before_add;
        bi_add->rhs.s_tag = LSR;
        bi_add->rhs.s_value = 31;
        bi_add->dst = make_operand((Value *)bi_I, mb);
        push((MI *)bi_add, mb);
        MOperand div_result = bi_add->dst;
        mul_info optmul = can_optmul(dividend);
        MOperand mul_result;
        if (optmul.c_1 == 2) {
            auto bi_add = new MI_Binary;
            bi_add->op = BINARY_ADD;
            bi_add->lhs = div_result;
            bi_add->rhs = div_result;
            bi_add->rhs.s_tag = LSL;
            bi_add->rhs.s_value = optmul.msb - optmul.s_val;
            bi_add->dst = make_vreg(vreg_count++);
            push((MI *)bi_add, mb);
            MOperand last;
            last = bi_add->dst;
            if (dividend < 0) {
                auto rsb = new MI_Binary;
                rsb->op = BINARY_RSB;
                rsb->lhs = bi_add->dst;
                rsb->rhs = make_imm(0);
                rsb->dst = rsb->lhs;
                push((MI *)rsb, mb);
                last = rsb->dst;
            }
            MOperand for_mul = last;
            if (optmul.s_val) {
                auto bi_lsl = new MI_Binary;
                bi_lsl->op = BINARY_LSL;
                bi_lsl->lhs = last;
                bi_lsl->rhs = make_imm(optmul.s_val);
                bi_lsl->dst = make_vreg(vreg_count++);
                push((MI *)bi_lsl, mb);
                for_mul = bi_lsl->dst;
            }
            mul_result = for_mul;
        } else if (optmul.c_1 + optmul.s_val == optmul.msb + 1) {
            MOperand last;
            if (dividend > 0) {
                auto rsb = new MI_Binary;
                rsb->op = BINARY_RSB;
                rsb->lhs = div_result;
                rsb->rhs = rsb->lhs;
                rsb->rhs.s_tag = LSL;
                rsb->rhs.s_value = optmul.msb - optmul.s_val + 1;
                rsb->dst = make_vreg(vreg_count++);
                push((MI *)rsb, mb);
                last = rsb->dst;
            } else {
                auto sub = new MI_Binary;
                sub->op = BINARY_SUBTRACT;
                sub->lhs = div_result;
                sub->rhs = sub->lhs;
                sub->rhs.s_tag = LSL;
                sub->rhs.s_value = optmul.msb - optmul.s_val + 1;
                sub->dst = sub->lhs;
                push((MI *)sub, mb);
                last = sub->dst;
            }
            MOperand for_mul = last;
            if (optmul.s_val) {
                auto bi_lsl = new MI_Binary;
                bi_lsl->op = BINARY_LSL;
                bi_lsl->lhs = last;
                bi_lsl->rhs = make_imm(optmul.s_val);
                bi_lsl->dst = make_vreg(vreg_count++);
                push((MI *)bi_lsl, mb);
                for_mul = bi_lsl->dst;
            }
            mul_result = for_mul;
        } else {
            auto lhs = div_result;
            auto rhs = make_operand(bi_I->rhs->v, mb, true);
            auto dst = make_vreg(vreg_count++);
            auto bi = new MI_Binary(BINARY_MULTIPLY, dst, lhs, rhs);
            push((MI *)bi, mb);
            mul_result = dst;
        }
        auto bi_sub = new MI_Binary;
        bi_sub->op = BINARY_SUBTRACT;
        bi_sub->lhs = bi_smmul->lhs;
        bi_sub->rhs = mul_result;
        bi_sub->dst = make_operand((Value *)bi_I, mb);
        push((MI *)bi_sub, mb);
    }

    else if (bi_I->op_type == BINARY_MOD) {
        auto bi_divide = new MI_Binary;
        bi_divide->op = BINARY_DIVIDE;
        bi_divide->lhs = make_operand(bi_I->lhs->v, mb, true);
        bi_divide->rhs = make_operand(bi_I->rhs->v, mb, true);
        bi_divide->dst = make_vreg(vreg_count++);
        push((MI *)bi_divide, mb);
        auto bi_mul = new MI_Binary;
        bi_mul->op = BINARY_MULTIPLY;
        bi_mul->lhs = bi_divide->dst;
        bi_mul->rhs = bi_divide->rhs;
        bi_mul->dst = make_vreg(vreg_count++);
        push((MI *)bi_mul, mb);
        auto bi_sub = new MI_Binary;
        bi_sub->op = BINARY_SUBTRACT;
        bi_sub->lhs = bi_divide->lhs;
        bi_sub->rhs = bi_mul->dst;
        bi_sub->dst = make_operand((Value *)bi_I, mb);
        push((MI *)bi_sub, mb);
    } else if (bi_I->op_type == BINARY_DIVIDE && is_1) {
    } else if (bi_I->op_type == BINARY_DIVIDE && is_n1) {
        auto rsb = new MI_Binary;
        rsb->op = BINARY_RSB;
        rsb->lhs = make_operand(bi_I->lhs->v, mb, true);
        rsb->rhs = make_imm(0);
        rsb->dst = make_operand((Value *)bi_I, mb);
        push((MI *)rsb, mb);
    } else if (bi_I->op_type == BINARY_DIVIDE && p2) {
        auto dividend = make_operand(bi_I->lhs->v, mb, true);
        auto bi_asr = new MI_Binary;
        bi_asr->op = BINARY_ASR;
        bi_asr->lhs = dividend;
        bi_asr->rhs = make_imm(31);
        bi_asr->dst = make_vreg(vreg_count++);
        push((MI *)bi_asr, mb);
        auto bi_add = new MI_Binary;
        bi_add->op = BINARY_ADD;
        bi_add->lhs = dividend;
        bi_add->rhs = bi_asr->dst;
        bi_add->rhs.s_tag = LSR;
        bi_add->rhs.s_value = 32 - p2;
        bi_add->dst = make_vreg(vreg_count++);
        push((MI *)bi_add, mb);
        if (((Constant *)(bi_I->rhs->v))->value > 0) {
            auto bi_asr2 = new MI_Binary;
            bi_asr2->op = BINARY_ASR;
            bi_asr2->lhs = bi_add->dst;
            bi_asr2->rhs = make_imm(p2);
            bi_asr2->dst = make_operand((Value *)bi_I, mb);
            push((MI *)bi_asr2, mb);
        } else {
            auto mov = new MI_Move(make_vreg(vreg_count++), make_imm(0));
            push((MI *)mov, mb);
            auto sub = new MI_Binary;
            sub->op = BINARY_SUBTRACT;
            sub->lhs = mov->dst;
            sub->rhs = bi_add->dst;
            sub->rhs.s_tag = ASR;
            sub->rhs.s_value = p2;
            sub->dst = make_operand((Value *)bi_I, mb);
            push((MI *)sub, mb);
        }
    } else if (bi_I->op_type == BINARY_DIVIDE && is_constant) {
        int32 dividend = ((Constant *)(bi_I->rhs->v))->value;
        assert(dividend < -1 || dividend > 1);
        ms magic_number;
        magic_number = magic(dividend);
        auto bi_smmul = new MI_Binary;
        bi_smmul->op = BINARY_SMMUL;
        bi_smmul->lhs = make_operand(bi_I->lhs->v, mb, true);
        bi_smmul->rhs = make_vreg(vreg_count++);
        bi_smmul->dst = make_vreg(vreg_count++);

        auto ldr = emit_load_of_constant(bi_smmul->rhs, magic_number.m);

        push(ldr, mb);
        push((MI *)bi_smmul, mb);

        MOperand smmul_next = bi_smmul->dst;
        if (dividend > 0 && magic_number.m < 0) {
            auto bi_add = new MI_Binary;
            bi_add->op = BINARY_ADD;
            bi_add->lhs = bi_smmul->dst;
            bi_add->rhs = bi_smmul->lhs;
            bi_add->dst = make_vreg(vreg_count++);
            push((MI *)bi_add, mb);
            smmul_next = bi_add->dst;
        }
        if (dividend < 0 && magic_number.m > 0) {
            auto sub = new MI_Binary;
            sub->op = BINARY_SUBTRACT;
            sub->lhs = bi_smmul->dst;
            sub->rhs = bi_smmul->lhs;
            sub->dst = make_vreg(vreg_count++);
            push((MI *)sub, mb);
            smmul_next = sub->dst;
        }
        MOperand before_add;
        before_add = smmul_next;
        if (magic_number.s > 0) {
            auto bi_asr = new MI_Binary;
            bi_asr->op = BINARY_ASR;
            bi_asr->lhs = smmul_next;
            bi_asr->rhs = make_imm(magic_number.s);
            bi_asr->dst = make_vreg(vreg_count++);
            push((MI *)bi_asr, mb);
            before_add = bi_asr->dst;
        }
        auto bi_add = new MI_Binary;
        bi_add->op = BINARY_ADD;
        bi_add->lhs = before_add;
        bi_add->rhs = before_add;
        bi_add->rhs.s_tag = LSR;
        bi_add->rhs.s_value = 31;
        bi_add->dst = make_operand((Value *)bi_I, mb);
        push((MI *)bi_add, mb);
    } else if (bi_I->op_type == BINARY_MULTIPLY && is_n1) {
        auto rsb = new MI_Binary;
        rsb->op = BINARY_RSB;
        rsb->lhs = make_operand(bi_I->lhs->v, mb, true);
        rsb->rhs = make_imm(0);
        rsb->dst = make_operand((Value *)bi_I, mb);
        push((MI *)rsb, mb);
    } else if (bi_I->op_type == BINARY_MULTIPLY && p2) {
        auto bi_lsl = new MI_Binary;
        bi_lsl->op = BINARY_LSL;
        bi_lsl->lhs = make_operand(bi_I->lhs->v, mb, true);
        bi_lsl->rhs = make_imm(p2);
        bi_lsl->dst = make_operand((Value *)bi_I, mb);
        push((MI *)bi_lsl, mb);
    } else if (bi_I->op_type == BINARY_MULTIPLY && is_constant) {
        mul_info optmul = can_optmul(((Constant *)(bi_I->rhs->v))->value);
        if (optmul.c_1 == 2) {
            auto bi_add = new MI_Binary;
            bi_add->op = BINARY_ADD;
            bi_add->lhs = make_operand(bi_I->lhs->v, mb, true);
            bi_add->rhs = bi_add->lhs;
            bi_add->rhs.s_tag = LSL;
            bi_add->rhs.s_value = optmul.msb - optmul.s_val;
            bi_add->dst = make_vreg(vreg_count++);
            push((MI *)bi_add, mb);
            MOperand last;
            last = bi_add->dst;
            if (((Constant *)(bi_I->rhs->v))->value < 0) {
                auto rsb = new MI_Binary;
                rsb->op = BINARY_RSB;
                rsb->lhs = bi_add->dst;
                rsb->rhs = make_imm(0);
                rsb->dst = make_vreg(vreg_count++);
                push((MI *)rsb, mb);
                last = rsb->dst;
            }
            if (optmul.s_val) {
                auto bi_lsl = new MI_Binary;
                bi_lsl->op = BINARY_LSL;
                bi_lsl->lhs = last;
                bi_lsl->rhs = make_imm(optmul.s_val);
                bi_lsl->dst = make_operand((Value *)bi_I, mb);
                push((MI *)bi_lsl, mb);
            } else
                value_map[bi_I] = last;
        } else if (optmul.c_1 + optmul.s_val == optmul.msb + 1) {
            MOperand last;
            if (((Constant *)(bi_I->rhs->v))->value > 0) {
                auto rsb = new MI_Binary;
                rsb->op = BINARY_RSB;
                rsb->lhs = make_operand(bi_I->lhs->v, mb, true);
                rsb->rhs = rsb->lhs;
                rsb->rhs.s_tag = LSL;
                rsb->rhs.s_value = optmul.msb - optmul.s_val + 1;
                rsb->dst = make_vreg(vreg_count++);
                push((MI *)rsb, mb);
                last = rsb->dst;
            } else {
                auto sub = new MI_Binary;
                sub->op = BINARY_SUBTRACT;
                sub->lhs = make_operand(bi_I->lhs->v, mb, true);
                sub->rhs = sub->lhs;
                sub->rhs.s_tag = LSL;
                sub->rhs.s_value = optmul.msb - optmul.s_val + 1;
                sub->dst = make_vreg(vreg_count++);
                push((MI *)sub, mb);
                last = sub->dst;
            }
            if (optmul.s_val) {
                auto bi_lsl = new MI_Binary;
                bi_lsl->op = BINARY_LSL;
                bi_lsl->lhs = last;
                bi_lsl->rhs = make_imm(optmul.s_val);
                bi_lsl->dst = make_operand((Value *)bi_I, mb);
                push((MI *)bi_lsl, mb);
            } else
                value_map[bi_I] = last;
        } else {
            auto lhs = make_operand(bi_I->lhs->v, mb, true);
            auto rhs = make_operand(bi_I->rhs->v, mb, true);
            auto dst = make_operand((Value *)bi_I, mb);
            auto bi = new MI_Binary(bi_I->op_type, dst, lhs, rhs);
            push((MI *)bi, mb);
        }
    } else {
        auto lhs = make_operand(bi_I->lhs->v, mb, true);
        MOperand rhs;
        if (bi_I->op_type == BINARY_MULTIPLY || bi_I->op_type == BINARY_DIVIDE)
            rhs = make_operand(bi_I->rhs->v, mb,
                               /* no imm for mul and sdiv */ true);
        else
            rhs = make_operand(bi_I->rhs->v, mb);

        auto dst = make_operand((Value *)bi_I, mb);
        auto bi = new MI_Binary(bi_I->op_type, dst, lhs, rhs);
        push((MI *)bi, mb);
    }
}

Program_Asm *emit_asm(Program_IR *program_IR, bool enable_optimization) {
    auto program_asm = new Program_Asm;
    opt = enable_optimization;

    // do only one for now
    // program_asm->functions.push(emit_function_asm(program_IR->procedures[0],
    // 0));
    int i = 0;
    for (auto func_IR : program_IR->procedures) {
        if (func_IR->is_builtin)
            continue;
        auto func_asm = emit_function_asm(func_IR, i++);
        program_asm->functions.push(func_asm);
    }

    return program_asm;
}

void emit_branch_asm(Func_Asm *func_asm, Binary_Op_Type op,
                     Instruction_Branch *br_I, Machine_Block *mb) {
    auto br = new MI_Branch();

    br->cond = binary_op_to_branch_cond(op);
    br->true_target = func_asm->mbs[br_I->true_target->index_in_procedure];
    br->false_target = func_asm->mbs[br_I->false_target->index_in_procedure];

    push((MI *)br, mb);
    mb->control_transfer_inst = (MI *)br;
}

void emit_relational_branch(Instruction_Binary *bi, Instruction_Branch *br,
                            Machine_Block *mb) {
    assert(is_relational_binary_op(bi->op_type));

    auto lhs = make_operand(bi->lhs->v, mb, true);
    auto rhs = make_operand(bi->rhs->v, mb);
    auto cmp = new MI_Compare(lhs, rhs);

    // @FIXME: don't use neg for now
    // as it has some problem with INT_MIN
    /*
    if (cmp->rhs.tag == IMM && cmp->rhs.value < 0) {
        cmp->rhs.value *= -1;
        cmp->neg = true; // cmn
    }
    */

    push(cmp, mb);

    emit_branch_asm(func_asm, bi->op_type, br, mb);
}

void emit_unary_branch(Instruction_Unary *un, Instruction_Branch *br,
                       Machine_Block *mb) {

    if (un->op_type == UNARY_NEGATIVE) {
        auto cmp =
            new MI_Compare(make_operand(un->oprend->v, mb, true), make_imm(0));
        push(cmp, mb);

        emit_branch_asm(func_asm, BINARY_NOT_EQUAL_TO, br, mb);

    } else if (un->op_type == UNARY_NOT) {
        auto cmp =
            new MI_Compare(make_operand(un->oprend->v, mb, true), make_imm(0));
        push(cmp, mb);

        emit_branch_asm(func_asm, BINARY_EQUAL_TO, br, mb);
    }
}

void emit_legacy(Value *I, Basic_Block *bb, Machine_Block *mb, int32 &i) {
    switch (I->type) {
    case INST_BRANCH: {
        auto br_I = (Instruction_Branch *)I;
        auto cmp =
            new MI_Compare(make_operand(br_I->cond->v, mb, true), make_imm(0));
        push((MI *)cmp, mb);
        emit_branch_asm(func_asm, BINARY_NOT_EQUAL_TO, br_I, mb);
    } break;

    case INST_UNARY: {
        auto un_I = (Instruction_Unary *)I;

        if (un_I->op_type == UNARY_NEGATIVE) {

            auto next_inst = bb->insts[i + 1];
            if (next_inst->type == INST_BRANCH) {
                auto cmp = new MI_Compare(
                    make_operand(un_I->oprend->v, mb, true), make_imm(0));
                push((MI *)cmp, mb);
                i++; // handle the branch IR at the same time

                emit_branch_asm(func_asm, BINARY_NOT_EQUAL_TO,
                                (Instruction_Branch *)next_inst, mb);
            } else {
                auto rsb = new MI_Binary();
                rsb->op = BINARY_RSB;
                rsb->dst = make_operand(I, mb);
                // @TODO: this is reversed...
                rsb->rhs = make_imm(0);
                rsb->lhs = make_operand(un_I->oprend->v, mb, true);
                push((MI *)rsb, mb);
            }

        } else if (un_I->op_type == UNARY_NOT) {

            auto next_inst = bb->insts[i + 1];
            if (next_inst->type == INST_BRANCH) {
                auto cmp = new MI_Compare(
                    make_operand(un_I->oprend->v, mb, true), make_imm(0));
                push((MI *)cmp, mb);
                i++; // handle the branch IR at the same time

                emit_branch_asm(func_asm, BINARY_EQUAL_TO,
                                (Instruction_Branch *)next_inst, mb);

            } else {

                // if (!x + !y)

                // clz r1, r0
                // lsr r2, r1, #5

                auto clz = new MI_Clz;
                clz->dst = make_vreg(vreg_count++);
                clz->operand = make_operand(un_I->oprend->v, mb, true);
                auto lsr = new MI_Binary;
                lsr->op = BINARY_LSR;
                lsr->dst = make_operand(un_I, mb);
                lsr->lhs = clz->dst;
                lsr->rhs = make_imm(5);

                push((MI *)clz, mb);
                push((MI *)lsr, mb);
            }

        } else {
            exit(50);
            assert(false);
        }
    } break;

    case INST_BINARY: {
        auto bi_I = (Instruction_Binary *)I;

        // binary inst can't be the last inst
        // since last inst must be branch/ret
        // so no need to bound check here.
        auto next_inst = bb->insts[i + 1];
        if (next_inst->type != INST_BRANCH) {
            emit_binary_value_naive(bi_I, mb);
        } else {
            // emit cmp instruction
            // for things like <, >, emit a cmp of lhs and rhs
            // for arithmteic values like add/constant,
            // emit the computation first, and compare it with 0
            // @TODO @Optimization: can use set flag to optimized away the cmp
            if (bi_I->op_type >= BINARY_NOT_EQUAL_TO &&
                bi_I->op_type <= BINARY_GREATER_THAN) {
                auto lhs = make_operand(bi_I->lhs->v, mb, true);
                auto rhs = make_operand(bi_I->rhs->v, mb);
                auto cmp = new MI_Compare(lhs, rhs);

                // @FIXME: don't use neg for now
                // as it has some problem with INT_MIN
                /*
                if (cmp->rhs.tag == IMM && cmp->rhs.value < 0) {
                    cmp->rhs.value *= -1;
                    cmp->neg = true; // cmn
                }
                */

                push((MI *)cmp, mb);
                i++; // handle the branch IR at the same time

                auto br_I = (Instruction_Branch *)next_inst;
                emit_branch_asm(func_asm, bi_I->op_type, br_I, mb);

            } else {
                emit_binary_value_naive(bi_I, mb);
                auto lhs = make_operand(bi_I, mb, true);
                auto cmp = new MI_Compare(lhs, make_imm(0));

                push((MI *)cmp, mb);
                i++; // handle the branch IR at the same time

                auto br_I = (Instruction_Branch *)next_inst;
                emit_branch_asm(func_asm, BINARY_NOT_EQUAL_TO, br_I, mb);
            }
        }
    } break;
    }
}

Func_Asm *emit_function_asm(Procedure_IR *IR, int32 i) {
    vreg_count = 0; // reset vreg count to 0 for each function
    func_asm = new Func_Asm;
    func_asm->has_return_value = IR->has_return_value;
    func_asm->index = i;
    func_asm->name = IR->name;
    for (auto v : IR->blocks[0]->insts) {
        if (v->type == GLOBAL) {
            Global *global = (Global *)v;
            func_asm->global_value.push(global->name);
        }
    }

    // DONT LOAD GLOBAL REFS AT ENTRY
    // IF THERE ARE TOO MANY GLOBALS
    // func_asm->too_many_globals = (func_asm->global_value.len > 5000);
    func_asm->too_many_globals = true;

    for (auto bb : IR->blocks) {
        func_asm->mbs.push(new Machine_Block());
        func_asm->mbs.back()->i = bb->index_in_procedure;
        func_asm->mbs.back()->loop_depth = bb->loop_depth;
        func_asm->mbs.back()->belongs_to_loop = bb->belongs_to_loop;
    }

    // copy CFG
    renumber_blocks(IR);
    for (auto bb : IR->blocks) {
        for (auto pred_bb : bb->preds) {
            auto from = func_asm->mbs[pred_bb->index_in_procedure];
            auto to = func_asm->mbs[bb->index_in_procedure];
            from->succs.push(to);
            to->preds.push(from);
        }
    }

    // copy first four arguments to vregs
    for (int i = 0; i < 4 && i < IR->arguments.len; i++) {
        auto arg = IR->arguments[i];

        if (!arg)
            continue; // arg is not used!

        assert(arg->arg_index >= 0 && arg->arg_index < 4);
        auto vreg = make_vreg(vreg_count++);
        value_map[arg] = vreg;

        emit_move(vreg, make_reg(arg->arg_index), func_asm->mbs[0]);
    }

    // translate each basic block to machine code
    // except for phi functions
    for (auto bb : IR->blocks) {
        Machine_Block *mb = func_asm->mbs[bb->index_in_procedure];
        for (int i = 0; i < bb->insts.len; i++) {
            auto I = bb->insts[i];
            switch (I->type) {

            case INST_BRANCH: {
                if (!opt) {
                    emit_legacy(I, bb, mb, i);
                    break;
                }

                auto br_I = (Instruction_Branch *)I;
                if (br_I->cond->v->type == INST_BINARY) {

                    auto bi = br_I->cond->v->as<Instruction_Binary>();
                    if (is_relational_binary_op(bi->op_type)) {
                        emit_relational_branch(bi, br_I, mb);
                    }

                } else if (br_I->cond->v->type == INST_UNARY) {

                    auto un = br_I->cond->v->as<Instruction_Unary>();
                    emit_unary_branch(un, br_I, mb);

                } else {
                    auto cmp = new MI_Compare(
                        make_operand(br_I->cond->v, mb, true), make_imm(0));
                    push((MI *)cmp, mb);
                    emit_branch_asm(func_asm, BINARY_NOT_EQUAL_TO, br_I, mb);
                }
            } break;

            case INST_UNARY: {
                if (!opt) {
                    emit_legacy(I, bb, mb, i);
                    break;
                }
                auto un_I = (Instruction_Unary *)I;

                auto next_inst = bb->insts[i + 1];
                auto br = next_inst->as<Instruction_Branch>();

                if (br && br->cond->v == un_I) {
                    emit_unary_branch(un_I, br, mb);
                    i++;
                } else {
                    switch (un_I->op_type) {
                    case UNARY_NEGATIVE: {
                        auto rsb = new MI_Binary();
                        rsb->op = BINARY_RSB;
                        rsb->dst = make_operand(I, mb);
                        // @TODO: this is reversed...
                        rsb->rhs = make_imm(0);
                        rsb->lhs = make_operand(un_I->oprend->v, mb, true);
                        push((MI *)rsb, mb);
                    } break;

                    case UNARY_NOT: {

                        // if (!x + !y)

                        // clz r1, r0
                        // lsr r2, r1, #5

                        auto clz = new MI_Clz;
                        clz->dst = make_vreg(vreg_count++);
                        clz->operand = make_operand(un_I->oprend->v, mb, true);
                        auto lsr = new MI_Binary;
                        lsr->op = BINARY_LSR;
                        lsr->dst = make_operand(un_I, mb);
                        lsr->lhs = clz->dst;
                        lsr->rhs = make_imm(5);

                        push((MI *)clz, mb);
                        push((MI *)lsr, mb);

                    } break;

                    default:
                        exit(50);
                        assert(false);
                    }
                }

            } break;

            case INST_BINARY: {
                if (!opt) {
                    emit_legacy(I, bb, mb, i);
                    break;
                }

                auto bi_I = (Instruction_Binary *)I;

                // binary inst can't be the last inst
                // since last inst must be branch/ret
                // so no need to bound check here.
                auto next_inst = bb->insts[i + 1];
                auto br = next_inst->as<Instruction_Branch>();
                if (br && br->cond->v == I) {
                    // emit cmp instruction
                    // for things like <, >, emit a cmp of lhs and rhs
                    // for arithmteic values like add/constant,
                    // emit the computation first, and compare it with 0
                    // @TODO @Optimization: can use set flag to optimized away
                    // the cmp
                    if (is_relational_binary_op(bi_I->op_type)) {

                        emit_relational_branch(bi_I, br, mb);
                        i++;

                    } else {

                        emit_binary_value(bi_I, mb);
                        auto cmp =
                            new MI_Compare(make_operand(bi_I, mb), make_imm(0));

                        push((MI *)cmp, mb);
                        i++; // handle the branch IR at the same time

                        auto br_I = (Instruction_Branch *)next_inst;
                        emit_branch_asm(func_asm, BINARY_NOT_EQUAL_TO, br_I,
                                        mb);
                    }

                } else if (!is_relational_binary_op(bi_I->op_type)) {
                    emit_binary_value(bi_I, mb);
                }

            } break;

            case INST_RETURN: {
                auto ret_I = (Instruction_Return *)I;
                auto ret = new MI_Return;

                if (ret_I->return_value) {
                    auto ret_value = make_operand(ret_I->return_value->v, mb);

                    emit_move(make_reg(r0), ret_value, mb);
                }

                push((MI *)ret, mb);

                mb->control_transfer_inst = (MI *)ret;
            } break;

            case INST_DIRECT_BRANCH: {
                auto br_I = (Instruction_Direct_Branch *)I;
                auto target_mb =
                    func_asm->mbs[br_I->target->index_in_procedure];
                auto br = new MI_Branch(NO_CONDITION, target_mb);
                push((MI *)br, mb);
                mb->control_transfer_inst = (MI *)br;
            } break;

            case FUNCTION_CALL: {
                auto func_I = (Function_Call *)I;
                auto call = new MI_Func_Call(func_I->name);
                call->arg_count = func_I->arguments.len;

                // prepare arguments before calling
                for (int i = 0; i < func_I->arguments.len; i++) {
                    auto arg_value = func_I->arguments[i]->v;

                    MOperand arg_operand;
                    if (i < 4) {
                        arg_operand = make_reg(i);
                        emit_move(arg_operand, make_operand(arg_value, mb), mb);
                    } else {
                        // move argument to the stack
                        auto str = new MI_Store;
                        str->mem_tag = MEM_PREP_ARG;
                        str->reg = make_operand(arg_value, mb, true);
                        str->base = make_reg(sp);
                        str->offset = make_imm((i - 4) * 4);
                        push((MI *)str, mb);
                    }
                }

                push((MI *)call, mb);

                if (func_I->has_return_value) {
                    auto result = make_operand(I, mb);
                    emit_move(result, make_reg(r0), mb);
                }
            } break;

            case GLOBAL: {
                if (opt) {
                    auto g = make_operand(I, mb);
                    emit_load_of_global_ref(func_asm, (Global *)I, g, mb);
                }
            } break;

            case ARGUMENT: {
                auto arg_v = (Argument *)I;
                if (opt && arg_v->arg_index >= 4) {
                    auto a = make_operand(I, mb);
                    emit_load_of_later_arg(arg_v, a, mb);
                }
            } break;

            case ALLOCA: {
                auto alloc = (Alloca *)I;
                auto const_value = ((Constant *)alloc->size->v)->value;
                func_asm->stack_size += const_value;
                auto bi_I = new MI_Binary;
                bi_I->op = BINARY_SUBTRACT;
                bi_I->lhs = make_reg(sp);
                bi_I->rhs = make_imm(func_asm->stack_size);
                bi_I->dst = make_operand(I, mb);
                func_asm->local_array_bases.push(bi_I);
                push((MI *)bi_I, mb);
            } break;

            case PHI:          // phis will be handled later
            case CONSTANT:     // @TODO: figure out how constants are loaded
            case MEMORY_DEF:   // memory defs and phis are useless after the IR
            case MEMORY_PHI: { /* Do nothing */
            } break;

            case MEMORY_READ: {
                auto Read_I = (Memory_Read *)I;
                auto ldr = new MI_Load;
                ldr->reg = make_operand(Read_I, mb, true);
                ldr->base = make_operand(Read_I->base->v, mb);
                if (Read_I->offset) {
                    if (Read_I->offset->v->type == CONSTANT) {
                        auto const_val = (Constant *)Read_I->offset->v;
                        ldr->offset = make_imm(const_val->value * 4);
                    } else {
                        ldr->offset = make_operand(Read_I->offset->v, mb);
                        ldr->offset.s_tag = LSL;
                        ldr->offset.s_value = 2;
                    }
                }
                push((MI *)ldr, mb);
            } break;
            case MEMORY_WRITE: {
                auto Write_I = (Memory_Write *)I;
                auto str = new MI_Store;
                str->reg = make_operand(Write_I->value_to_write->v, mb, true);
                str->base = make_operand(Write_I->base->v, mb);
                if (Write_I->offset) {
                    if (Write_I->offset->v->type == CONSTANT) {
                        auto const_val = (Constant *)Write_I->offset->v;
                        str->offset = make_imm(const_val->value * 4);
                    } else {
                        str->offset = make_operand(Write_I->offset->v, mb);
                        str->offset.s_tag = LSL;
                        str->offset.s_value = 2;
                    }
                }
                push((MI *)str, mb);
            } break;
            default: {
                exit(51);
                assert(false &&
                       "translating unknown IR instructions to assembly");
            }
            }
        }
    }

    // handle phi functions
    for (auto bb : IR->blocks) {
        auto mb = func_asm->mbs[bb->index_in_procedure];
        for (int i = 0; i < bb->insts.len; i++) {
            if (bb->insts[i]->type == PHI) {
                auto phi = (Phi *)bb->insts[i];
                auto incoming = make_vreg(vreg_count++);

                auto phi_as_operand = make_operand(phi, mb);
                auto mv = emit_move(phi_as_operand, incoming);
                insert((MI *)mv, mb->inst);

                for (int p = 0; p < phi->operands.len; p++) {
                    auto phi_opr = phi->operands[p];
                    auto pred_bb = bb->preds[p];
                    auto pred_mb = func_asm->mbs[pred_bb->index_in_procedure];

                    auto mv =
                        emit_move(incoming, make_operand(phi_opr->v, pred_mb));
                    if (pred_mb->control_transfer_inst) {
                        insert((MI *)mv, pred_mb->control_transfer_inst);
                    } else {
                        push((MI *)mv, pred_mb);
                    }
                }
            }
        }
    }
    func_asm->vreg_count = vreg_count;
    return func_asm;
}

void build_globals(String_Builder *s, Array<Ast_Declaration *> globals) {
    if (globals.len == 0)
        return;
    s->append(".data\n.align 2\n");
    for (int i = 0; i < globals.len; i++) {
        if (globals[i]->decl_type->tag == TYPE_ARRAY) {
            auto dims = ((Type_Info_Array *)globals[i]->decl_type)->dimension;
            auto init_list = (Ast_Init_List *)globals[i]->initial_value;

            if (init_list && init_list->list.len != 0) {
                s->append("%s:\n", globals[i]->id->name);
                auto init_inst = get_init_sequence(dims, init_list);
                for (int j = 0; j < init_inst.len; j++) {
                    s->append("    ");
                    if (init_inst[j].tag == LONG) {
                        int32 c =
                            evaluate(NULL, init_inst[j].init_value, globals);
                        s->append(".word  %d\n", c);
                    } else {
                        s->append(".zero  %u\n", init_inst[j].num_of_zeros * 4);
                    }
                }
            } else {
                continue;
            }
        }
        if (globals[i]->decl_type->tag == TYPE_INTEGER) {
            s->append("%s:\n", globals[i]->id->name);
            s->append("    ");
            if (globals[i]->initial_value)
                s->append(
                    ".word  %d\n",
                    ((Ast_Integer_Literal *)globals[i]->initial_value)->value);
            else
                s->append(".word  %d\n", 0);
        }
    }
    s->append(".bss\n");
    s->append(".align 2\n");
    for (int i = 0; i < globals.len; i++) {
        if (globals[i]->decl_type->tag == TYPE_ARRAY) {
            auto dims = ((Type_Info_Array *)globals[i]->decl_type)->dimension;
            auto init_list = (Ast_Init_List *)globals[i]->initial_value;
            if (init_list && init_list->list.len != 0)
                continue;

            init_list = new Ast_Init_List;

            int num_of_zeros =
                get_init_sequence(dims, init_list)[0].num_of_zeros * 4;
            s->append("%s:\n", globals[i]->id->name);
            s->append("    .space %d\n", num_of_zeros);
            s->append("    .type %s, %%object\n", globals[i]->id->name);
            s->append("    .size %s, %d\n", globals[i]->id->name, num_of_zeros);
        }
    }
}

void print_operand(MOperand op) {
    String_Builder s;
    build_operand(&s, op);
    printf("%s", s.c_str());
}

void build_operand(String_Builder *s, MOperand op) {
    switch (op.tag) {
    case REG: {
        if (op.value == fp)
            s->append("fp");
        else if (op.value == sp)
            s->append("sp");
        else if (op.value == lr)
            s->append("lr");
        else if (op.value == pc)
            s->append("pc");
        else
            s->append("r%d", op.value);
    } break;

    case VREG: {
        s->append("vr%d", op.value);
    } break;
    case IMM: {
        s->append("#%d", op.value);
    } break;
    case ADR_GLOBAL: {
        s->append("%s", op.adr);
    } break;
    case SHAYEBUSHI: {
        exit(52);
        assert(false);
    } break;
    }
    switch (op.s_tag) {
    case Nothing:
        break;
    case LSL: {
        s->append(", lsl #%d", op.s_value);
    } break;
    case LSR: {
        s->append(", lsr #%d", op.s_value);
    } break;
    case ASL: {
        s->append(", asl #%d", op.s_value);
    } break;
    case ASR: {
        s->append(", asr #%d", op.s_value);
    } break;
    }
}

void print_function_asm(Func_Asm *func) {
    String_Builder s;
    build_function_asm(&s, func);
    printf("%s", s.c_str());
}

void build_function_asm(String_Builder *s, Func_Asm *func) {
    s->append("%s:\n", func->name);
    for (int i = 0; i < func->mbs.len; i++) {
        Machine_Block *next_bb = NULL;
        if (i != func->mbs.len - 1)
            next_bb = func->mbs[i + 1];
        s->append(".L%d_%d:\n", func->index, func->mbs[i]->i);
        for (auto I = func->mbs[i]->inst; I; I = I->next) {

            s->append("    ");
            const char *cond = get_branch_suffix(I->cond);
            const char *set_flags = I->update_flags ? "s" : "";
            switch (I->tag) {
            case MI_MOVE: {
                auto mv = (MI_Move *)I;
                if (mv->neg) {
                    s->append("mvn%s%s ", set_flags, cond);
                } else {
                    s->append("mov%s%s ", set_flags, cond);
                }
                build_operand(s, mv->dst);
                s->append(", ");
                build_operand(s, mv->src);
            } break;

            case MI_CLZ: {
                auto clz = (MI_Clz *)I;
                s->append("clz%s%s ", set_flags, cond);
                build_operand(s, clz->dst);
                s->append(", ");
                build_operand(s, clz->operand);
            } break;

            case MI_BINARY: {
                auto bi = (MI_Binary *)I;

                switch (bi->op) {
                case BINARY_ADD: {
                    s->append("add");
                } break;
                case BINARY_SUBTRACT: {
                    s->append("sub");
                } break;
                case BINARY_MULTIPLY: {
                    s->append("mul");
                } break;
                case BINARY_DIVIDE: {
                    s->append("sdiv");
                } break;
                case BINARY_LSL: {
                    s->append("lsl");
                } break;
                case BINARY_LSR: {
                    s->append("lsr");
                } break;
                case BINARY_ASL: {
                    s->append("asl");
                } break;
                case BINARY_ASR: {
                    s->append("asr");
                } break;
                case BINARY_RSB: {
                    s->append("rsb");
                } break;
                case BINARY_BITWISE_AND: {
                    s->append("and");
                } break;
                case BINARY_BITWISE_OR: {
                    s->append("orr");
                } break;
                case BINARY_BIC: {
                    s->append("bic");
                } break;
                case BINARY_SMMUL: {
                    s->append("smmul");
                } break;
                default: {
                    exit(53);
                    assert(false && "unknown binary asm instruction.");
                }
                }
                s->append("%s%s", set_flags, cond);
                s->append(" ");
                build_operand(s, bi->dst);
                s->append(", ");
                build_operand(s, bi->lhs);
                s->append(", ");
                build_operand(s, bi->rhs);
            } break;

            case MI_COMPARE: {
                auto cmp = (MI_Compare *)I;

                if (cmp->neg) {
                    s->append("cmn ");
                } else {
                    s->append("cmp ");
                }
                build_operand(s, cmp->lhs);
                s->append(", ");
                build_operand(s, cmp->rhs);

            } break;

            case MI_BRANCH: {
                auto br = (MI_Branch *)I;
                if (next_bb && next_bb->condified)
                    break;
                if (br->cond == NO_CONDITION) {
                    if (br->true_target->i != i + 1) {
                        s->append("b .L%d_%d", func->index, br->true_target->i);
                    }
                } else if (br->true_target->i == i + 1) {
                    // invert the branch cond
                    s->append("b%s .L%d_%d\n",
                              get_branch_suffix(invert_branch_cond(br->cond)),
                              func->index, br->false_target->i);
                } else if (br->false_target->i == i + 1) {
                    s->append("b%s .L%d_%d\n", get_branch_suffix(br->cond),
                              func->index, br->true_target->i);
                } else {
                    s->append("b%s .L%d_%d\n", get_branch_suffix(br->cond),
                              func->index, br->true_target->i);
                    s->append("    ");
                    s->append("b .L%d_%d", func->index, br->false_target->i);
                }
            } break;

            case MI_PUSH:
            case MI_POP: {
                auto push_or_pop = (MI_Push *)I;

                s->append((I->tag == MI_PUSH) ? "push {" : "pop {");
                for (int i = 0; i < push_or_pop->operands.len; i++) {
                    build_operand(s, push_or_pop->operands[i]);
                    if (i != push_or_pop->operands.len - 1)
                        s->append(", ");
                }
                s->append("}");

            } break;

            case MI_FUNC_CALL: {
                auto call = (MI_Func_Call *)I;
                s->append("bl%s %s", cond, call->func_name);
            } break;

            case MI_LOAD:
            case MI_STORE: {
                auto load_or_store = (MI_Load *)I;

                if (load_or_store->base.tag == ADR_GLOBAL) {
                    s->append("movw%s ", cond);
                    build_operand(s, load_or_store->reg);
                    s->append(", ");
                    s->append(":lower16:");
                    build_operand(s, load_or_store->base);
                    s->append("\n    movt%s ", cond);
                    build_operand(s, load_or_store->reg);
                    s->append(", ");
                    s->append(":upper16:");
                    build_operand(s, load_or_store->base);
                } else if (load_or_store->base.tag == IMM) {
                    auto val = load_or_store->base.value;
                    if (can_be_imm_ror(val)) {
                        s->append("mov%s ", cond);
                        build_operand(s, load_or_store->reg);
                        s->append(", ");
                        build_operand(s, load_or_store->base);
                    } else if ((val & 0xFFFF) == val) {
                        s->append("movw%s ", cond);
                        build_operand(s, load_or_store->reg);
                        s->append(", ");
                        build_operand(s, load_or_store->base);
                    } else if (val < 0 && val > -258) {
                        s->append("mvn%s ", cond);
                        build_operand(s, load_or_store->reg);
                        s->append(", ");
                        auto valn = -val - 1;
                        MOperand valn_imm(IMM, valn);
                        build_operand(s, valn_imm);
                    } else {
                        auto vall = val & 0xFFFF;
                        auto valh = (val >> 16) & 0xFFFF;
                        MOperand vall_imm(IMM, vall);
                        MOperand valh_imm(IMM, valh);
                        s->append("movw%s ", cond);
                        build_operand(s, load_or_store->reg);
                        s->append(", ");
                        build_operand(s, vall_imm);
                        s->append("\n    movt%s ", cond);
                        build_operand(s, load_or_store->reg);
                        s->append(", ");
                        build_operand(s, valh_imm);
                    }
                } else {
                    s->append(I->tag == MI_LOAD ? "ldr" : "str");
                    s->append(cond);
                    s->append(" ");
                    build_operand(s, load_or_store->reg);
                    s->append(", [");
                    build_operand(s, load_or_store->base);
                    if (load_or_store->offset.tag != SHAYEBUSHI) {
                        s->append(", ");
                        build_operand(s, load_or_store->offset);
                    }
                    s->append("]");
                }
            } break;

            case MI_RETURN: {
                s->append("bx lr");
            } break;
            default: {
                exit(54);
                assert(false && "printing unknown assembly instruction.");
            } break;
            }
            s->append("\n");
        }
    }
}

void print_program_asm(Program_Asm *pro, Array<Ast_Declaration *> globals) {
    String_Builder s;
    build_program_asm(&s, pro, globals);
    printf("%s", s.c_str());
}

void build_program_asm(String_Builder *s, Program_Asm *pro,
                       Array<Ast_Declaration *> globals) {

    s->append(".arch armv8-a\n");
    s->append(".arch_extension crc\n");

    build_globals(s, globals);
    s->append("\n");

    s->append(".text\n");
    s->append(".align 2\n");
    s->append(".syntax unified\n");
    s->append(".arm\n");

    s->append(".global main\n\n");
    for (int i = 0; i < pro->functions.len; i++) {
        build_function_asm(s, pro->functions[i]);
        s->append("\n\n");
    }
}

// https://stackoverflow.com/a/18215803/4568242
bool is_callee_save(uint8 reg) {
    return (reg >= r4 && reg <= r11) || reg == lr;
}

bool is_caller_save(uint8 reg) {
    return (reg >= r0 && reg <= r3) || reg == r12;
}
