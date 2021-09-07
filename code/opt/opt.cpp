
#include "opt.h"

// Optimization passes
#include "verify_ir.h"
#include "dce.h"
#include "dead_code_elimination.h"
#include "sccp.h"
#include "simplify_cfg.h"
#include "loop_code_motion.h"
#include "compute_dominator.h"
#include "compute_loops.h"
#include "bake_constant.h"
#include "callgraph.h"
#include "memory_ssa.h"
#include "gvn_gcm.h"
#include "copy_function.h"
#include "inlining.h"
#include "loop_unrolling.h"
#include "remove_unused_functions.h"
#include "mem2reg_for_acc.h"
#include "memory_copy_propagation.h"
#include "sysy_craziness.h"
#include "inst_comb.h"

inline void run_on_every_function(Program_IR *IR, Opt_Func_Pass pass) {
    for(auto f : IR->procedures) {
        if (f->is_builtin) continue;
        pass(f);
    }
}

#define RUN(pass_name) pass_name(IR)
void perform_magic(Program_IR *IR) {
    RUN(sysy_craziness);
    RUN(bake_constant);
    RUN(gvn_gcm);
    RUN(loop_unrolling);
    RUN(memory_ssa);
    RUN(gvn_gcm);

    RUN(mem2reg_for_acc);
    RUN(inline_functions);

    RUN(memory_ssa);
    RUN(mem_copy_propagation);
    RUN(simplify_cfg);
    RUN(gvn_gcm);

    RUN(simplify_cfg);
    RUN(inst_comb);
    RUN(remove_unused_functions);
}
#undef RUN
