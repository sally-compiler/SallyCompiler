#include "asm_passes.h"

// this pass spills all vregs to the stack

namespace STACK_RA {

/* @note: STACK LAYOUT
 *
 * -------------------
 *  arguments passed in
 * ------------------- <- "frame potiner"
 *  callee-save regs
 * -------------------
 *  local arrays
 * -------------------
 *  spilled variables
 * ------------------- <- spill_base
 *  arguments when calling other functions
 * ------------------- <- sp
 */

void stack_ra_on_function(Func_Asm *f) {

    uint32 callee_size = 0, local_array_size = f->stack_size,
           spilled_size = f->vreg_count * 4, arg_size = 0;

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

                auto str = new MI_Store;
                str->mem_tag = MEM_SAVE_SPILL;
                str->reg = make_reg(r4);
                str->base = make_reg(sp);
                str->offset = make_imm(arg_size + def.value * 4);

                insert(str, I->next);
                replace_defs(I, def, str->reg);
            }

            auto uses = get_uses(I, f->has_return_value);
            uint8 r = r5; // using r5~r7 as temp

            // @TODO uses might be the same??
            for (auto use : uses) {
                if (use.tag != VREG)
                    continue;

                assert(r >= r5 && r <= r7);
                auto ldr = new MI_Load;
                ldr->mem_tag = MEM_LOAD_SPILL;
                ldr->reg = make_reg(r++);
                ldr->base = make_reg(sp);
                ldr->offset = make_imm(arg_size + use.value * 4);

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

    insert(sub_sp, f->mbs[0]->inst);
    insert(push, f->mbs[0]->inst);

    for (auto bb : f->mbs) {
        if (bb->last_inst && bb->last_inst->tag == MI_RETURN) {
            auto pop = new MI_Pop;
            for (uint8 r = r0; r < REG_COUNT; r++) {
                if (is_callee_save(r) || r == lr) {
                    pop->operands.push(make_reg(r));
                }
            }
            insert(add_sp, bb->last_inst);
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

} // namespace STACK_RA

ASM_PASS(stack_ra) {
    run_on_every_function(program_asm, STACK_RA::stack_ra_on_function);
}
