#ifdef ENABLE_CFG_VIEWER

#include <gvc.h>
#include <stdio.h>
#include <string>

#include "ir.h"

Agnode_t *make_graph_node(graph_t *g, std::string content, std::string comments,
                          bool has_branch = false) {
    Agnode_t *n = agnode(g, NULL, 1);

    content = "{{" + content + '|' + comments + "}";
    if (has_branch) {
        content += "|{<s0>true|<s1>false}";
    }
    content += '}';

    agsafeset((void *)n, "label", (char *)(content.c_str()), "");
    agsafeset((void *)n, "shape", "record", "");
    agsafeset((void *)n, "fontname", "Cambria Math", "");
    // agsafeset((void *)n, "fontname", "Comic Sans", "");

    return n;
}

enum Edge_Type { TRUE_TARGET, FALSE_TARGET, NO_COND };

void add_edge(graph_t *g, Agnode_t *from, Agnode_t *to, Edge_Type type) {
    Agedge_t *e = agedge(g, from, to, NULL, 1);

    if (type != NO_COND) {
        agsafeset((void *)e, "tailport", (type == TRUE_TARGET) ? "s0" : "s1",
                  "");
    }

    // agsafeset((void *)e, "headport", "n", ""); ugly
}

void make_cfg(Array<Basic_Block *> basic_blocks) {
    GVC_t *gvc = gvContext();
    graph_t *g = agopen("g", Agdirected, 0);

    // make CFG nodes
    Array<Agnode_t *> nodes;
    for (auto bb : basic_blocks) {
        auto bb_content = print_basic_block(bb, true);
        auto bb_comments = print_comments(bb);
        bool has_branch = (bb->insts.len != 0) &&
                          (bb->insts[bb->insts.len - 1]->type == INST_BRANCH);
        nodes.push(make_graph_node(g, bb_content, bb_comments, has_branch));
    }

    // connect the nodes
    for (auto bb : basic_blocks) {
        for (auto parent_bb : bb->preds) {
            auto parent_node = nodes[parent_bb->index_in_procedure];
            auto child_node = nodes[bb->index_in_procedure];
            Edge_Type type = NO_COND;

            if (parent_bb->insts.len != 0) {
                auto last_inst = parent_bb->insts[parent_bb->insts.len - 1];
                bool has_branch = (last_inst->type == INST_BRANCH);
                if (has_branch) {
                    auto br_inst = (Instruction_Branch *)last_inst;
                    type = (br_inst->true_target == bb) ? TRUE_TARGET
                                                        : FALSE_TARGET;
                }
            }

            add_edge(g, parent_node, child_node, type);
        }
    }

    gvLayout(gvc, g, "dot");
    // gvRender(gvc, g, "dot", stdout);
    gvRenderFilename(gvc, g, "pdf", "build/ir.pdf");
    gvRenderFilename(gvc, g, "png", "build/ir.png");

    gvFreeLayout(gvc, g);
    agclose(g);
    gvFreeContext(gvc);
}

#endif
