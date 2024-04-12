#include "opt.h"

namespace LCM {
#include "finddominator.h"

void LCM_on_single_function(Procedure_IR *IR) { invariant_code_motion(IR); }

} // namespace LCM

OPT_PASS(loop_code_motion) {
    run_on_every_function(IR, LCM::LCM_on_single_function);
}