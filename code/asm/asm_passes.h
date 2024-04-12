#ifndef ASM_PASSES_H
#define ASM_PASSES_H

// @Lame
// This is copied from IR passes

#include "../arm.h"
#include "../general.h"

#define ASM_FUNC_PASS(pass_name) void pass_name(Func_Asm *func_asm)
typedef ASM_FUNC_PASS(Asm_Func_Pass);

#define ASM_PASS(pass_name) void pass_name(Program_Asm *program_asm)
typedef ASM_PASS(Asm_Pass);

inline void run_on_every_function(Program_Asm *program_asm, Asm_Func_Pass pass);

void bless(Program_Asm *program_asm, bool opt = false);

void replace_uses(MI *I, MOperand old_opr, MOperand new_opr);
void replace_defs(MI *I, MOperand old_opr, MOperand new_opr);
Array<MOperand> get_defs(MI *I);
Array<MOperand> get_uses(MI *I, bool func_has_return_value);

#endif
