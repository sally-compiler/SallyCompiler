#include "opt.h"

namespace LOOP {

void compute_loop_body(Procedure_IR *f, Loop *l);

// travel the CFG in a depth first way.
// record the order blocks are discovered by
// assigning them a number from 0~NB-1
// if an edge that goes from high number to low number is found
// and the low number bb dominates the high number bb
// A loop is found
void dfs(Procedure_IR *f, Basic_Block *bb, Array<int> &num,
         Array<Loop *> &loops, int i = 0) {
    if (bb_visited(bb))
        return;
    // printf("visiting B%d with number %d\n", bb->index_in_procedure, i);
    num[bb->index_in_procedure] = i;
    mark_visited(bb);

    for (auto child : bb->succs) {
        dfs(f, child, num, loops, i + 1);
    }

    for (auto pred : bb->preds) {
        // assert(bb_visited(pred));
        if (bb_visited(pred) &&
            num[pred->index_in_procedure] > num[bb->index_in_procedure]) {
            if (pred->dominators.find(bb) != -1) {
                auto loop = new Loop;
                loop->header = bb;
                loop->exit = pred;

                loops.push(loop);
                compute_loop_body(f, loop);
                // printf("found loop (entry, exit) = (B%d, B%d)\n",
                // loop->header->index_in_procedure,
                // loop->exit->index_in_procedure);
            }
        }
    }
}

void compute_loop_body(Procedure_IR *f, Loop *l) {
    l->body.push(l->header);

    // go up from loop exit block until we see header
    Map<Basic_Block *, bool> vis;
    Queue<Basic_Block *> q;
    q.push(l->exit);

    while (!q.empty()) {
        auto bb = q.front();
        q.pop();
        if (vis[bb])
            continue;
        if (bb == l->header)
            continue;
        l->body.push(bb);

        if (bb->loop != NULL) {
            bb->loop->parent = l;
        }
        bb->loop = l;
        vis[bb] = true;

        for (auto pred : bb->preds) {
            q.push(pred);
        }
    }

    printf("loop (entry, exit) = (B%d, B%d) has body (",
           l->header->index_in_procedure, l->exit->index_in_procedure);
    for (auto bb : l->body) {
        printf("%d ", bb->index_in_procedure);
    }
    printf(")\n");
}

void compute_loop_depth(Procedure_IR *f, Array<Loop *> &loops) {

    if (loops.len == 0)
        return;

    for (int i = loops.len - 1; i >= 0; i--) {
        auto header = loops[i]->header;
        loops[i]->is_innermost = true;

        bool nested = false;
        for (int j = i + 1; j < loops.len; j++) {
            if (loops[j]->body.find(header) != -1) {
                // header is nested inside the previous loop
                // or... they share a header
                nested = true;
                if (loops[j]->header == header) {
                    loops[i]->depth = loops[j]->depth;
                } else {
                    loops[i]->depth = loops[j]->depth + 1;
                    loops[j]->is_innermost = false;
                }
                break;
            }
        }

        if (!nested) {
            loops[i]->depth = 1;
        }
    }

    for (int i = loops.len - 1; i >= 0; i--) {
        for (auto bb : loops[i]->body) {
            // @note: this if statement handles this case
            // while(1) {
            //      if (sth) continue
            //      while(1) {}
            //      if (srh) continue
            // }
            if (loops[i]->depth > bb->loop_depth) {
                bb->loop_depth = loops[i]->depth;
                bb->belongs_to_loop = i;
            }
        }
    }

    for (auto loop : loops) {
        printf(
            "loop (entry, exit) = (B%d, B%d) has depth %d, is_innermost: %d\n",
            loop->header->index_in_procedure, loop->exit->index_in_procedure,
            loop->depth, loop->is_innermost);
    }

    printf("\n");
}

void compute_function_loops(Procedure_IR *f) {

    printf(">>> computing loops for function %s\n:", f->name);

    // @TODO: Do not use mark for visited flag
    // as mark might have many other uses
    mark_all_blocks_unvisited(f->blocks);
    for (auto bb : f->blocks)
        bb->belongs_to_loop = -1;
    for (auto bb : f->blocks)
        bb->loop_depth = 0;
    for (auto bb : f->blocks)
        bb->loop = NULL;

    Array<int> num(f->blocks.len);
    Array<Loop *> loops;
    dfs(f, f->start_block, num, loops);
    compute_loop_depth(f, loops);

    for (auto l : loops) {
        if (l->parent) {
            l->parent->sub_loops.push(l);
        }
    }

    f->loops = loops;
}

} // namespace LOOP

OPT_PASS(compute_loops) {
    run_on_every_function(IR, LOOP::compute_function_loops);
}
