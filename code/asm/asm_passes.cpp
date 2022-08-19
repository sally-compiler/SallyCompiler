
#include "asm_passes.h"
#include "../arm.h"

// Optimization passes
#include "block_placement.h"
#include "condify.h"
#include "linear_scan.h"
#include "register_allocation.h"
#include "remove_identical_moves.h"
#include "remove_redundant_ldrs.h"
#include "simplify_asm.h"
#include "stack_ra.h"

inline void run_on_every_function(Program_Asm *program_asm,
                                  Asm_Func_Pass pass) {
    for (auto func_asm : program_asm->functions) {
        pass(func_asm);
    }
}

#define RUN(pass_name) pass_name(program_asm)
void bless(Program_Asm *program_asm, bool opt) {
    RUN(remove_redundant_ldrs);
    // RUN(linear_scan);
    register_allocation(program_asm, opt);
    RUN(remove_identical_moves);
    // RUN(stack_ra);
    if (opt) {
        RUN(simplify_asm);
        RUN(remove_redundant_ldrs);
        RUN(block_placement);
        RUN(condify);
    }
}
#undef RUN

// helper functions

void replace_uses(MI *I, MOperand old_opr, MOperand new_opr) {
    Array<MOperand *> oprs;

    switch (I->tag) {

    case MI_MOVE: {
        oprs.push(&((MI_Move *)I)->src);
    } break;

    case MI_CLZ: {
        oprs.push(&((MI_Clz *)I)->operand);
    } break;

    case MI_BINARY: {
        oprs.push(&((MI_Binary *)I)->lhs);
        oprs.push(&((MI_Binary *)I)->rhs);
    } break;

    case MI_COMPARE: {
        oprs.push(&((MI_Compare *)I)->lhs);
        oprs.push(&((MI_Compare *)I)->rhs);
    } break;

    // no need to handle function call
    case MI_FUNC_CALL: {

    } break;

    case MI_STORE: {
        oprs.push(&((MI_Load *)I)->reg);
        oprs.push(&((MI_Store *)I)->base);
        oprs.push(&((MI_Store *)I)->offset);
    } break;

    case MI_LOAD: {
        oprs.push(&((MI_Load *)I)->base);
        oprs.push(&((MI_Load *)I)->offset);
    } break;

    /*
    case MI_PUSH: {
        auto push = (MI_Push *) I;
        for(int i = 0; i < push->operands.len; i++) {
            oprs.push(&push->operands[i]);
        }
    } break;
    */

    // also no need for return
    case MI_RETURN:
    case MI_POP:
    case MI_PUSH:
    case MI_BRANCH:
        break;

    default:
        exit(60);
        assert(false);
    }

    for (MOperand *opr_ptr : oprs) {
        if (opr_ptr && *opr_ptr == old_opr) {
            // @NOTE! remember to keep lsl info!!!
            opr_ptr->tag = new_opr.tag;
            opr_ptr->value = new_opr.value;
            opr_ptr->adr = new_opr.adr;
            //*opr_ptr = new_opr;
        }
    }
}

void replace_defs(MI *I, MOperand old_opr, MOperand new_opr) {
    Array<MOperand *> oprs;
    switch (I->tag) {
    case MI_MOVE: {
        oprs.push(&((MI_Move *)I)->dst);
    } break;
    case MI_BINARY: {
        oprs.push(&((MI_Binary *)I)->dst);
    } break;

    // no need to do function call
    case MI_FUNC_CALL: {
    } break;

    // bx lr
    case MI_RETURN: {
    } break;

    case MI_LOAD: {
        oprs.push(&((MI_Load *)I)->reg);
    } break;

    case MI_CLZ: {
        oprs.push(&((MI_Clz *)I)->dst);
    } break;

        /*
        case MI_POP:  {
            auto pop = (MI_Pop *) I;
            for(int i = 0; i < pop->operands.len; i++) {
                oprs.push(&pop->operands[i]);
            }
        } break;
        */

    case MI_COMPARE:
    case MI_PUSH:
    case MI_POP:
    case MI_STORE: // @TODO: store/load may use self increment thing?
    case MI_BRANCH:
        break;

    default:
        exit(61);
        assert(false);
    }

    for (MOperand *opr_ptr : oprs) {
        if (opr_ptr && *opr_ptr == old_opr) {
            *opr_ptr = new_opr;
        }
    }
}

Array<MOperand> get_defs(MI *I) {
    Array<MOperand> oprs;
    switch (I->tag) {
    case MI_MOVE: {
        oprs.push(((MI_Move *)I)->dst);
    } break;
    case MI_BINARY: {
        oprs.push(((MI_Binary *)I)->dst);
    } break;
    case MI_FUNC_CALL: {
        oprs.push(make_reg(lr));
        for (uint8 r = r0; r < REG_COUNT; r++) {
            if (is_caller_save(r)) {
                oprs.push(make_reg(r));
            }
        }
    } break;

    case MI_LOAD: {
        oprs.push(((MI_Load *)I)->reg);
    } break;

    case MI_CLZ: {
        oprs.push(((MI_Clz *)I)->dst);
    } break;

        /*
        case MI_POP:  {
            auto pop = (MI_Pop *) I;
            for (auto pop_operand : pop->operands) {
                oprs.push(pop_operand);
            }
        } break;
        */

    case MI_COMPARE:
    case MI_PUSH:
    case MI_RETURN:
    case MI_POP:
    case MI_STORE: // @TODO: store/load may use self increment thing?
    case MI_BRANCH:
        break;

    default:
        exit(62);
        assert(false);
    }
    return oprs;
}

Array<MOperand> get_uses(MI *I, bool func_has_return_value) {
    Array<MOperand> oprs;

    switch (I->tag) {

    case MI_MOVE: {
        oprs.push(((MI_Move *)I)->src);
    } break;

    case MI_CLZ: {
        oprs.push(((MI_Clz *)I)->operand);
    } break;

    case MI_BINARY: {
        oprs.push(((MI_Binary *)I)->lhs);
        oprs.push(((MI_Binary *)I)->rhs);
    } break;

    case MI_COMPARE: {
        oprs.push(((MI_Compare *)I)->lhs);
        oprs.push(((MI_Compare *)I)->rhs);
    } break;

    case MI_FUNC_CALL: {
        auto call = (MI_Func_Call *)I;
        // @TODO: get uses based on arg count
        for (uint8 r = r0; r <= r3; r++) {
            if (r >= call->arg_count)
                break;
            oprs.push(make_reg(r));
        }
    } break;

    case MI_STORE: {
        oprs.push(((MI_Load *)I)->reg);
        oprs.push(((MI_Store *)I)->base);
        oprs.push(((MI_Store *)I)->offset);
    } break;

    case MI_LOAD: {
        oprs.push(((MI_Load *)I)->base);
        oprs.push(((MI_Load *)I)->offset);
    } break;

        /*
        case MI_PUSH: {
            auto push = (MI_Push *) I;
            for (auto push_operand : push->operands) {
                oprs.push(push_operand);
            }
        } break;
        */

    case MI_RETURN: {
        oprs.push(make_reg(lr));
        if (func_has_return_value) {
            oprs.push(make_reg(r0));
        }
    }

    case MI_POP:
    case MI_PUSH:
    case MI_BRANCH:
        break;

    default:
        exit(63);
        assert(false);
    }

    return oprs;
}
