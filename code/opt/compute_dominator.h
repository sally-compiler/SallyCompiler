#include "opt.h"

namespace DOM {

Array<Array<bool>>
compute_dominator_matrix(Procedure_IR *func_IR,
                         bool compute_post_dominator = false) {

    int NB = func_IR->blocks.len; // number of basic blocks

    auto dominated_by = make_2d<bool>(NB, NB);

    Basic_Block *top =
        (compute_post_dominator ? func_IR->exit_block : func_IR->start_block);
    int32 top_i = top->index_in_procedure;

    // Initialized to N for all other blocks
    for (int i = 0; i < NB; i++)
        for (int j = 0; j < NB; j++)
            dominated_by[i][j] = 1;

    for (int i = 0; i < NB; i++)
        dominated_by[top_i][i] = 0;
    dominated_by[top_i][top_i] = 1;

    Array<bool> temp(NB);
    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = NB - 1; i >= 0; i--) {
            if (i == top_i)
                continue;
            for (int j = 0; j < NB; j++)
                temp[j] = 1;

            Array<Basic_Block *> bbs =
                (compute_post_dominator ? (func_IR->blocks[i]->succs)
                                        : (func_IR->blocks[i]->preds));
            for (auto bb : bbs) {
                int k = bb->index_in_procedure;
                for (int m = 0; m < NB; m++) {
                    temp[m] &= dominated_by[k][m];
                }
            }

            temp[i] = 1;

            bool same = true;
            for (int j = 0; j < NB; j++) {
                if (temp[j] != dominated_by[i][j]) {
                    same = false;
                }
                dominated_by[i][j] = temp[j];
            }

            if (!same)
                changed = true;
        }
    }

    return dominated_by;
}

void compute_dominator_on_single_function(Procedure_IR *func_IR) {

    int32 NB = func_IR->blocks.len;

    auto dom_matrix = compute_dominator_matrix(func_IR);

    // attach dominator information to each basic block
    for (auto bb : func_IR->blocks) {
        bb->dominators.release();
        int32 bb_i = bb->index_in_procedure;
        for (int i = 0; i < func_IR->blocks.len; i++) {
            if (dom_matrix[bb_i][i]) {
                bb->dominators.push(func_IR->blocks[i]);
            }
        }
    }

    // compute dom depth and immediate dominator for each block
    // by running BFS
    Array<Array<Basic_Block *>> dom_tree;
    dom_tree.set_len(func_IR->blocks.len);

    for (int i = NB - 1; i >= 0; i--) {
        auto b1 = func_IR->blocks[i];
        auto b1_doms = b1->dominators;
        for (int j = NB - 1; j >= 0; j--) {
            if (i == j)
                continue;
            auto b2 = func_IR->blocks[j];
            auto b2_doms = b2->dominators;
            if (b1_doms.len - 1 == b2_doms.len) {
                int same = true;
                for (int k = 0; k < b1_doms.len; k++) {
                    // @Performance
                    if (b1_doms[k] == b1)
                        continue;
                    if (b2_doms.find(b1_doms[k]) == -1) {
                        // if (b1_doms[k] != b2_doms[k]) {
                        same = false;
                        break;
                    }
                }
                if (same) {
                    printf("B%d is immediately dominated by B%d\n",
                           b1->index_in_procedure, b2->index_in_procedure);
                    b1->imm_dom = b2;
                    dom_tree[j].push(func_IR->blocks[i]);
                }
            }
        }
    }

    // compute dom depths
    Queue<Basic_Block *> q;
    q.push(func_IR->start_block);
    func_IR->start_block->dom_depth = 0;

    mark_all_blocks_unvisited(func_IR->blocks);

    while (!q.empty()) {
        auto bb = q.front();
        q.pop();

        for (auto child_bb : dom_tree[bb->index_in_procedure]) {
            if (!bb_visited(child_bb)) {
                mark_visited(child_bb);
                child_bb->dom_depth = bb->dom_depth + 1;
                q.push(child_bb);
            }
        }
    }

    // @DEBUG: printing debug dom info
    printf(">>> Computing dominator info...\n");
    for (auto bb : func_IR->blocks) {
        if (!bb->imm_dom) {
            printf("B%d(depth %d) is dominated by:", bb->index_in_procedure,
                   bb->dom_depth);
        } else {
            printf("B%d(imm_dom B%d, depth %d) is dominated by:",
                   bb->index_in_procedure, bb->imm_dom->index_in_procedure,
                   bb->dom_depth);
        }
        for (auto dom : bb->dominators) {
            printf(" B%d", dom->index_in_procedure);
        }
        printf("\n");
    }
}

}; // namespace DOM

OPT_PASS(compute_dominator) {
    run_on_every_function(IR, DOM::compute_dominator_on_single_function);
}
