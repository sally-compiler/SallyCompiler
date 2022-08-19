#ifndef OPT_H
#define OPT_H

#include "../general.h"
#include "../ir.h"

struct Loop {
    int32 depth = -1;
    Basic_Block *header = NULL;
    Basic_Block *exit = NULL;
    bool is_innermost = false;
    Loop *parent = NULL;
    Array<Loop *> sub_loops;
    Array<Basic_Block *> body;
};

// @TODO: passes might share information
// e.g. SCCP can find dead branches like if (false) ...
// and mark the corresponding flow edge as dead
// then simplify cfg pass comes along to actually eliminate that edge
//
// there are two places that live information can go
// 1) a set of live flow edges in Procedure_IR (IR side)
// 2) same set, but in opt.h, maybe in Opt_Context?
//

/* @TODO: passes can call each other? */

#define OPT_FUNC_PASS(pass_name) void pass_name(Procedure_IR *func_IR)
typedef OPT_FUNC_PASS(Opt_Func_Pass);

#define OPT_PASS(pass_name) void pass_name(Program_IR *IR)
typedef OPT_PASS(Opt_Pass);

inline void run_on_every_function(Program_IR *IR, Opt_Func_Pass pass);

// @Cleanup
/* Helper function for travelling in the CFG */
inline void mark_all_blocks_unvisited(Array<Basic_Block *> blocks) {
    for (auto bb : blocks) {
        bb->mark = 0;
    }
}

inline bool bb_visited(Basic_Block *bb) { return bb->mark; }

inline void mark_visited(Basic_Block *bb) { bb->mark = 1; }

inline void mark_all_values_as_unvisted(Procedure_IR *func_IR) {
    for (auto bb : func_IR->blocks) {
        for (auto v : bb->insts) {
            v->visited = false;
        }
    }
}

inline bool is_function_recursive(Procedure_IR *f) {
    return (f->caller.find(f) != -1);
}

void perform_magic(Program_IR *IR);

#endif
