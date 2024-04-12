#include "opt.h"

/* @note
 * this pass finds all constant globals that aren't declared with 'const'
 * as the int seed[3] in bitset
 */

bool is_constexpr(Ast_Node *n) {
    if (n->type == Ast_Type_Identifier)
        return true;

    assert(n->type == Ast_Type_Array_Reference);

    auto arr_ref = (Ast_Array_Reference *)n;
    for (int i = 0; i < arr_ref->indices.len; i++) {
        if (arr_ref->indices[i]->type != Ast_Type_Integer_Literal) {
            return false;
        }
    }
    return true;
}

namespace BAKE_CONST {

void find_const_globals(Program_IR *prog) {

    // if a global is really const?
    Map<Ast_Declaration *, bool> is_variable;

    // mark const global
    for (auto f : prog->procedures) {
        for (auto bb : f->blocks) {
            for (auto I : bb->insts) {
                if (auto mem_write = I->as<Memory_Write>()) {
                    auto glo_val = mem_write->base->v->as<Global>();
                    if (!glo_val)
                        continue;

                    is_variable[glo_val->decl] = true;
                } else if (auto call = I->as<Function_Call>()) {
                    // alias problem
                    // an global array can be passed to an function, which might
                    // modified its content

                    // go over the argument list to see there are global values
                    // unfortunately, there could be partial refs, sigh...
                    for (int i = 0; i < call->arguments.len; i++) {
                        auto arg_val = call->arguments[i]->v;

                        Global *glo_ref = arg_val->as<Global>();

                        // is arg_val parital ref?
                        if (!glo_ref) {
                            if (auto add = arg_val->as<Instruction_Binary>()) {
                                if (add->op_type == BINARY_ADD) {
                                    glo_ref = add->lhs->v->as<Global>();
                                }
                            }
                        }

                        // this argument takes a reference of global!
                        // we don't care whether that function modifies the
                        // content in the pointer or not just mark the global
                        // variable
                        if (glo_ref) {
                            printf("potential aliasing for %s, marking as "
                                   "variable\n",
                                   glo_ref->decl->id->name);
                            is_variable[glo_ref->decl] = true;
                        }
                    }
                }
            }
        }
    }

    // replace memory read of const global with actual Constant value
    // hope dce will clean up the rest
    for (auto f : prog->procedures) {
        for (auto bb : f->blocks) {
            for (int i = 0; i < bb->insts.len; i++) {
                auto I = bb->insts[i];

                auto mem_read = I->as<Memory_Read>();
                if (!mem_read)
                    continue;

                auto glo_val = mem_read->base->v->as<Global>();
                if (!glo_val)
                    continue;

                if (is_variable[glo_val->decl])
                    continue;

                if (!is_constexpr(mem_read->source))
                    continue;

                int const_to_be_baked =
                    evaluate(NULL, mem_read->source, prog->globals);
                auto const_val = new Constant(const_to_be_baked);

                I->replace_inplace(const_val);
            }
        }
    }
}

} // namespace BAKE_CONST

OPT_PASS(bake_constant) { BAKE_CONST::find_const_globals(IR); }
