
#include "asm_passes.h"
#include <algorithm>
#include <cmath>

namespace RA {

// liveIn and liveOut for each machine blocks
static Array<Set<MOperand>> live_in;
static Array<Set<MOperand>> live_out;

static Array<Set<MOperand>> use_set; // actually 'use without def' set
static Array<Set<MOperand>> def_set;

void analyse_liveness(Func_Asm *func_asm) {

    live_in.release();
    live_out.release();
    use_set.release();
    def_set.release();

    live_in.set_len(func_asm->mbs.len);
    live_out.set_len(func_asm->mbs.len);
    use_set.set_len(func_asm->mbs.len);
    def_set.set_len(func_asm->mbs.len);

    // get use and def sets for each machine block
    for (int i = 0; i < func_asm->mbs.len; i++) {
        for (auto I = func_asm->mbs[i]->inst; I; I = I->next) {
            Array<MOperand> defs = get_defs(I);
            Array<MOperand> uses = get_uses(I, func_asm->has_return_value);

            for (auto use : uses) {
                if (use.tag == VREG &&
                    def_set[i].find(use) == def_set[i].end()) {
                    use_set[i].insert(use);
                }
            }

            for (auto def : defs) {
                def_set[i].insert(def);
            }
        }
    }

    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = 0; i < func_asm->mbs.len; i++) {
            Set<MOperand> old_in = live_in[i];
            Set<MOperand> old_out = live_out[i];

            // live_in[i] = use_set[i] union (live_out[i]-def_set[i])
            live_in[i] = use_set[i];
            for (auto o : live_out[i]) {
                if (def_set[i].find(o) == def_set[i].end()) {
                    live_in[i].insert(o);
                }
            }

            live_out[i].clear();
            for (auto succ : func_asm->mbs[i]->succs) {
                for (auto succ_in : live_in[succ->i]) {
                    live_out[i].insert(succ_in);
                }
            }

            if (!changed) {
                if ((live_in[i] != old_in) || (live_out[i] != old_out)) {
                    changed = true;
                }
            }
        }
    }
}

const int K = 14; // r0~r12
Set<MOperand> precolored;
Set<MOperand> initial;
Set<MOperand> simplify_worklist;
Set<MOperand> freeze_worklist;
Set<MOperand> spill_worklist;
Set<MOperand> spilled_nodes;
Set<MOperand> coalesced_nodes;
Set<MOperand> colored_nodes;
Set<MOperand> select_set;
Array<MOperand> select_stack;
Array<uint8> needs_save;

// keep track of already spilled nodes
// avoid spilling repeatly
Set<MOperand> already_spilled;

Set<MI_Move *, MI_Move_Pointer_Cmp> coalesced_moves;
Set<MI_Move *, MI_Move_Pointer_Cmp> constrained_moves;
Set<MI_Move *, MI_Move_Pointer_Cmp> frozen_moves;
Set<MI_Move *, MI_Move_Pointer_Cmp> worklist_moves;
Set<MI_Move *, MI_Move_Pointer_Cmp> active_moves;

typedef Pair<MOperand, MOperand> Edge;
Set<Edge> adj_set;
// Set< Set<MOperand> > adj_list;
Map<MOperand, Set<MOperand>> adj_list;
Map<MOperand, int> degree; // @default 0?

Map<MOperand, Set<MI_Move *, MI_Move_Pointer_Cmp>> move_list;
Map<MOperand, MOperand> alias;
Map<MOperand, int32> color;
Map<MOperand, uint32> use_def_count;
Map<MOperand, uint32> loop_depth;

int32 spilled_stack_size = 0;
bool done_post_work = false;

#define in(it, set) (((set).find(it)) != ((set).end()))

bool need_alloc(MOperand opr) { return (opr.tag == REG || opr.tag == VREG); }

void add_edge(MOperand u, MOperand v);
void build(Func_Asm *func_asm);
Set<MOperand> adjacent(MOperand n);
// Set<MI_Move *> node_moves(MOperand n);
bool move_related(MOperand n);
void make_worklist();
void enable_moves(Set<MOperand> nodes);
void decrement_degree(MOperand m);
void simplify();
void coalesce();
void add_worklist(MOperand u);
bool ok(MOperand t, MOperand r);
bool conservative(Set<MOperand> nodes);
MOperand get_alias(MOperand n);
void combine(MOperand u, MOperand v);
void freeze();
void freeze_moves(MOperand u);
void assign_colors();
void select_spill();

void add_edge(MOperand u, MOperand v) {
    auto p = Edge{u, v};
    if (!in(p, adj_set) && !(u == v)) {
        // printf("interference: {"); print_operand(u); printf(", ");
        // print_operand(v); printf("}\n");
        adj_set.insert({u, v});
        adj_set.insert({v, u});

        if (u.tag != REG) {
            adj_list[u].insert(v);
            degree[u]++;
        }

        if (v.tag != REG) {
            adj_list[v].insert(u);
            degree[v]++;
        }
    }
}

void build(Func_Asm *func_asm) {
    for (auto b : func_asm->mbs) {
        auto live = live_out[b->i];
        for (auto I = func_asm->mbs[b->i]->last_inst; I; I = I->prev) {
            auto defs = get_defs(I);
            auto uses = get_uses(I, func_asm->has_return_value);
            if (I->tag == MI_MOVE) {
                if (need_alloc(defs[0]) && need_alloc(uses[0])) {
                    for (auto use : uses) {
                        if (!need_alloc(use))
                            continue;
                        live.erase(use);
                        move_list[use].insert((MI_Move *)I);
                    }

                    for (auto def : defs) {
                        move_list[def].insert((MI_Move *)I);
                    }

                    worklist_moves.insert((MI_Move *)I);
                }
            }

            // @Bug, needs to be allocated?
            for (auto def : defs) {
                if (need_alloc(def)) {
                    live.insert(def);
                    use_def_count[def]++;
                    if (b->loop_depth > loop_depth[def]) {
                        loop_depth[def] = b->loop_depth;
                    }
                    for (auto l : live)
                        add_edge(l, def);
                }
            }

            // @Optimize
            Set<MOperand> new_live;
            for (auto u : uses) {
                if (need_alloc(u)) {
                    use_def_count[u]++;
                    if (b->loop_depth > loop_depth[u]) {
                        loop_depth[u] = b->loop_depth;
                    }
                    new_live.insert(u);
                }
            }
            for (auto l : live) {
                // if (need_alloc(def) && l != def) {
                if (defs.find(l) == -1) {
                    new_live.insert(l);
                }
            }
            live = new_live;
        }
    }
}

Set<MOperand> adjacent(MOperand n) {
    Set<MOperand> a;
    auto it = adj_list[n].begin();
    while (it != adj_list[n].end()) {
        if (!in(*it, select_set) && !in(*it, coalesced_nodes)) {
            a.insert(*it);
        }
        it++;
    }
    return a;
}

Set<MI_Move *> node_moves(MOperand n) {
    Set<MI_Move *> s;
    auto it = move_list[n].begin();
    while (it != move_list[n].end()) {
        if (in(*it, active_moves) || in(*it, worklist_moves)) {
            s.insert(*it);
        }
        it++;
    }
    return s;
}

bool move_related(MOperand n) { return !node_moves(n).empty(); }

void make_worklist() {
    while (!initial.empty()) {
        auto n = *(initial.begin());
        initial.erase(n);
        if (degree[n] >= K) {
            spill_worklist.insert(n);
        } else if (move_related(n)) {
            freeze_worklist.insert(n);
        } else {
            simplify_worklist.insert(n);
        }
    }
}

void enable_moves(Set<MOperand> nodes) {
    for (auto n : nodes) {
        for (auto m : node_moves(n)) {
            if (in(m, active_moves)) {
                active_moves.erase(m);
                worklist_moves.insert(m);
            }
        }
    }
}

void decrement_degree(MOperand m) {
    int d = degree[m]--;
    if (d == K) {
        auto adj = adjacent(m);
        adj.insert(m);
        enable_moves(adj);
        spill_worklist.erase(m);
        if (move_related(m)) {
            freeze_worklist.insert(m);
        } else {
            simplify_worklist.insert(m);
        }
    }
}

void simplify() {
    auto n = *(simplify_worklist.begin());
    simplify_worklist.erase(n);
    // printf("simplifying "); print_operand(n); printf("\n");
    select_stack.push(n);
    select_set.insert(n);
    for (auto m : adjacent(n)) {
        decrement_degree(m);
    }
}

void coalesce() {
    auto m = *(worklist_moves.begin());
    worklist_moves.erase(m);
    // printf("coalescing "); print_operand(m->dst); printf(" <- ");
    // print_operand(m->src); printf("\n");
    auto u = get_alias(m->src);
    auto v = get_alias(m->dst);
    auto e = Edge{u, v};
    if (v.tag == REG) { // @What?
        auto t = u;
        u = v;
        v = t;
    }

    // @Performance
    int okok = false;
    for (auto t : adjacent(v)) {
        if (ok(t, u)) {
            okok = true;
            break;
        }
    }

    auto join = adjacent(u);
    for (auto n : adjacent(v)) {
        join.insert(n);
    }

    if (u == v) {
        coalesced_moves.insert(m);
        add_worklist(u);
    } else if (v.tag == REG || in(e, adj_set)) {
        constrained_moves.insert(m);
        add_worklist(u);
        add_worklist(v);
    } else if ((u.tag == REG && okok) || (u.tag != REG && conservative(join))) {
        coalesced_moves.insert(m);
        combine(u, v);
        add_worklist(u);
    } else {
        active_moves.insert(m);
    }
}

void add_worklist(MOperand u) {
    if (u.tag != REG && !move_related(u) && degree[u] < K) {
        freeze_worklist.erase(u);
        simplify_worklist.insert(u);
    }
}

bool ok(MOperand t, MOperand r) {
    auto p = Edge{t, r};
    return ((degree[t] < K) || (t.tag == REG) || (in(p, adj_set)));
}

bool conservative(Set<MOperand> nodes) {
    int k = 0;
    for (auto n : nodes) {
        if (degree[n] >= K) {
            k++;
        }
    }
    return k < K;
}

MOperand get_alias(MOperand n) {
    if (in(n, coalesced_nodes)) {
        return get_alias(alias[n]);
    } else {
        return n;
    }
}

void combine(MOperand u, MOperand v) {
    if (in(v, freeze_worklist)) {
        freeze_worklist.erase(v);
    } else {
        spill_worklist.erase(v);
    }
    coalesced_nodes.insert(v);
    alias[v] = u;

    // FIXME
    for (auto n : move_list[v]) {
        move_list[u].insert(n);
    }

    for (auto t : adjacent(v)) {
        add_edge(t, u);
        decrement_degree(t);
    }

    if (degree[u] >= K && in(u, freeze_worklist)) {
        freeze_worklist.erase(u);
        spill_worklist.insert(u);
    }
}

void freeze() {
    auto u = *(freeze_worklist.begin());
    freeze_worklist.erase(u);
    simplify_worklist.insert(u);
    freeze_moves(u);
}

void freeze_moves(MOperand u) {
    for (auto m : node_moves(u)) {
        // @Order?
        auto u = m->src;
        auto v = m->dst;

        if (in(m, active_moves)) {
            active_moves.erase(m);
        } else {
            worklist_moves.erase(m);
        }

        frozen_moves.insert(m);
        if (node_moves(v).empty() && degree[v] < K) {
            freeze_worklist.erase(v);
            simplify_worklist.insert(v);
        }
    }
}

// @Optimization: can we use lr?
bool is_general_purpose_register(uint8 r) {
    return ((r >= r0) && (r <= r12)) || r == lr;
    // return ((r >= r0) && (r <= r12)) || (r == lr);
    // return ((r >= r0) && (r <= r11));
}

void assign_colors() {
    while (!select_stack.empty()) {
        auto n = select_stack.back();
        select_stack.pop();
        Array<uint8> ok_colors;

        // assign caller save first
        ok_colors.push(r0);
        ok_colors.push(r1);
        ok_colors.push(r2);
        ok_colors.push(r3);
        ok_colors.push(r12);
        for (uint8 r = r4; r <= r11; r++) {
            ok_colors.push(r);
        }
        ok_colors.push(lr);

        for (auto w : adj_list[n]) {
            auto a = get_alias(w);
            if (in(a, colored_nodes) || in(a, precolored)) {
                ok_colors.remove(color[a]);
            }
        }
        if (ok_colors.empty()) {
            spilled_nodes.insert(n);
        } else {
            colored_nodes.insert(n);
            auto c = ok_colors.front();
            color[n] = c;
        }
    }
    for (auto n : coalesced_nodes) {
        color[n] = color[get_alias(n)];
    }
}

void select_spill() {

    // auto m = *(spill_worklist.begin());
    MOperand m = {};
    real32 min_cost = 10000000;
    for (auto it = spill_worklist.begin(); it != spill_worklist.end(); it++) {

        if (in(*it, already_spilled))
            continue;

        assert(degree[*it] != 0);

        real32 cost =
            use_def_count[*it] * pow(10, loop_depth[*it]) / degree[*it];
        print_operand(*it);
        printf(": use_def_count: %d, loop_depth: %d, degree: %d, cost: %f\n",
               use_def_count[*it], loop_depth[*it], degree[*it], cost);

        if (cost < min_cost) {
            m = *it;
            min_cost = cost;
        }
    }

    if (m.tag == SHAYEBUSHI)
        exit(90);
    // assert(m.tag != SHAYEBUSHI);

    spill_worklist.erase(m);
    simplify_worklist.insert(m);
    freeze_moves(m);
}

void color_register(MOperand &m) {
    if (need_alloc(m)) {
        uint8 reg = color[m];
        m.value = reg;
        m.tag = REG;
    }

    if (m.tag == REG && is_callee_save(m.value)) {
        if (needs_save.find(m.value) == -1) {
            needs_save.push(m.value);
        }
    }
}

void allocate_register(Func_Asm *func_asm) {

    precolored.clear();
    initial.clear();
    simplify_worklist.clear();
    freeze_worklist.clear();
    spill_worklist.clear();
    spilled_nodes.clear();
    coalesced_nodes.clear();
    colored_nodes.clear();
    coalesced_moves.clear();
    constrained_moves.clear();
    frozen_moves.clear();
    select_set.clear();
    worklist_moves.clear();
    active_moves.clear();
    adj_set.clear();
    adj_list.clear();
    degree.clear();
    move_list.clear();
    alias.clear();
    color.clear();
    use_def_count.clear();
    loop_depth.clear();

    select_stack.len = 0;
    needs_save.len = 0;

    analyse_liveness(func_asm);
    /*
printf(">>> Analysing liveness...\n\n");
for (int i = 0; i < func_asm->mbs.len; i++) {
   printf("MB%d:\n", i);

   printf("    live in: ");
   for(auto in : live_in[i]) {
       print_operand(in);
       printf(" ");
   }
   printf("\n");

   printf("    live out: ");
   for(auto out : live_out[i]) {
       print_operand(out);
       printf(" ");
   }
   printf("\n");
}
printf("\n");
*/

    // @Check: precolored register?
    for (int i = 0; i < REG_COUNT; i++) {
        auto r = make_reg(i);
        precolored.insert(r);
        color[r] = i;
    }

    for (int i = 0; i < func_asm->vreg_count; i++) {
        initial.insert(make_vreg(i));
    }

    printf(">>> Building interference graph for coloring...\n");
    build(func_asm);
    printf(">>> done building\n");
    make_worklist();
    printf(">>> worklist done\n");
    do {
        if (!simplify_worklist.empty())
            simplify();
        else if (!worklist_moves.empty())
            coalesce();
        else if (!freeze_worklist.empty())
            freeze();
        else if (!spill_worklist.empty())
            select_spill();
    } while (!simplify_worklist.empty() || !worklist_moves.empty() ||
             !freeze_worklist.empty() || !spill_worklist.empty());

    printf("assigning\n");
    assign_colors();
    printf("assigned\n");

    printf("\n");
    if (!spilled_nodes.empty()) {
        // rewrite_program(spilled_nodes);

        // rewrite program
        // insert stores and restores for each spilled node
        for (MOperand n : spilled_nodes) {

            already_spilled.insert(n);

            printf("spilling nodes: ");
            print_operand(n);
            printf("\n");
            spilled_stack_size += 4; // Alignment

            for (auto bb : func_asm->mbs) {
                for (auto I = bb->inst; I; I = I->next) {
                    auto defs = get_defs(I);
                    auto uses = get_uses(I, func_asm->has_return_value);

                    bool inserted_store = false;
                    bool inserted_use = false;
                    // insert store after def
                    if (defs.find(n) != -1) {
                        // if def is a load of global reference, an is spilled
                        // don't load at entry
                        // load at each use site instead
                        /*
                        if (I->tag == MI_LOAD && ((MI_Load *)I)->mem_tag ==
                        MEM_LOAD_GLOBAL_REF) { spilled_stack_size -= 4; auto
                        ldr_global = (MI_Load *) I; I->mark(); // mark as dead

                            for(auto bb : func_asm->mbs) {
                                for(auto I=bb->inst; I; I=I->next) {
                                    auto uses = get_uses(I,
                        func_asm->has_return_value); if (uses.find(n) != -1) {
                                        auto ldr = new MI_Load;
                                        ldr->mem_tag = MEM_LOAD_GLOBAL_REF;
                                        ldr->reg =
                        make_vreg(func_asm->vreg_count++); ldr->base =
                        ldr_global->base;

                                        replace_uses(I, n, ldr->reg);
                                        insert((MI *)ldr, I);

                                    }
                                }
                            }
                            continue;

                        }
                        */
                        auto str = new MI_Store;
                        str->mem_tag = MEM_SAVE_SPILL;
                        str->reg = make_vreg(func_asm->vreg_count++);
                        str->base = make_reg(sp);
                        str->offset = make_imm(
                            -(func_asm->stack_size + spilled_stack_size));
                        already_spilled.insert(str->reg);

                        replace_defs(I, n, str->reg);
                        insert((MI *)str, I->next); // @Last?
                        I = I->next; // jump past newly inserted str
                        inserted_store = true;
                    }

                    // insert restore before use
                    if (uses.find(n) != -1) {
                        auto ldr = new MI_Load;
                        ldr->mem_tag = MEM_LOAD_SPILL;
                        ldr->reg = make_vreg(func_asm->vreg_count++);
                        ldr->base = make_reg(sp);
                        ldr->offset = make_imm(
                            -(func_asm->stack_size + spilled_stack_size));
                        already_spilled.insert(ldr->reg);
                        replace_uses(I, n, ldr->reg);
                        // @TODO: get uses based on arg count
                        insert((MI *)ldr, I);
                        inserted_use = true;
                    }
                    assert(!(inserted_use && inserted_store));
                    if (inserted_use && inserted_store) {
                        exit(64);
                    }
                }
                // bb->erase_marked_values();
            }
        }

        printf(">>> spilled stack size: %d\n", spilled_stack_size);
        printf(">>> after spilling: \n");
        print_function_asm(func_asm);
        printf("\n");

        allocate_register(func_asm);
    } else {

        // @HACK
        // we need to know needs_save to setup the stack
        // which is computed by color_register
        // which runs after we setup the stack...
        for (auto bb : func_asm->mbs) {
            for (auto I = bb->inst; I; I = I->next) {
                for (auto d : get_defs(I)) {
                    color_register(d);
                }
            }
        }

        if (!done_post_work) {
            done_post_work = true;

            func_asm->stack_size += spilled_stack_size;

            printf(">>> registers that need to save: ");

            for (auto r : needs_save) {
                printf("r%d ", r);
            }
            printf("\n");

            // allocate stack space for argument passing
            int max_arg_count = 0;
            for (auto mb : func_asm->mbs) {
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
                func_asm->stack_size += (max_arg_count - 4) * 4;
            }

            // insert add/sub to set up or destroy the stack
            if (func_asm->stack_size != 0) {
                auto alloc_stack =
                    new MI_Binary(BINARY_SUBTRACT, make_reg(sp), make_reg(sp),
                                  make_imm(func_asm->stack_size));

                auto start_inst = func_asm->mbs[0]->inst;
                insert((MI *)alloc_stack, start_inst);

                for (auto bb : func_asm->mbs) {
                    if (bb->last_inst && bb->last_inst->tag == MI_RETURN) {
                        auto destroy_stack = new MI_Binary(
                            BINARY_ADD, make_reg(sp), make_reg(sp),
                            make_imm(func_asm->stack_size));

                        insert((MI *)destroy_stack, bb->last_inst);
                    }
                }
            }

            // convert fp to sp
            // now func_asm->stack_size = size of (local arrays + spilled vars +
            // max calling arguments); not including the callee-save registers'
            // space

            // fixup sp when calculating base address for local arrays
            for (auto base : func_asm->local_array_bases) {
                assert(base->tag == MI_BINARY);
                auto sub = (MI_Binary *)base;
                assert(sub->op == BINARY_SUBTRACT);
                assert(sub->lhs == make_reg(sp));
                assert(sub->rhs.tag == IMM && sub->rhs.value > 0);

                int32 offset_relative_to_sp =
                    func_asm->stack_size - sub->rhs.value;
                // @TODO replace with a mov directly
                // if offset relative to sp is 0

                sub->op = BINARY_ADD;
                sub->lhs = make_reg(sp);
                sub->rhs = make_imm(offset_relative_to_sp);
            }

            for (auto bb : func_asm->mbs) {
                for (auto I = bb->inst; I; I = I->next) {

                    int save_stack_size = needs_save.len * 4;
                    if (I->tag == MI_LOAD) {
                        auto ldr = (MI_Load *)I;
                        if (ldr->mem_tag == MEM_LOAD_SPILL) {
                            int32 offset_value =
                                ((ldr->offset.tag == SHAYEBUSHI)
                                     ? 0
                                     : ldr->offset.value);
                            int32 offset_relative_to_sp =
                                func_asm->stack_size + offset_value;
                            ldr->base.value = sp;
                            ldr->offset = make_imm(offset_relative_to_sp);
                        } else if (ldr->mem_tag == MEM_LOAD_ARG) {
                            int32 offset_value =
                                ((ldr->offset.tag == SHAYEBUSHI)
                                     ? 0
                                     : ldr->offset.value);
                            int32 offset_relative_to_sp = func_asm->stack_size +
                                                          offset_value +
                                                          save_stack_size;
                            ldr->base.value = sp;
                            ldr->offset = make_imm(offset_relative_to_sp);
                        }
                    }

                    if (I->tag == MI_STORE) {
                        auto str = (MI_Store *)I;
                        if (str->mem_tag == MEM_SAVE_SPILL) {
                            int32 offset_value =
                                ((str->offset.tag == SHAYEBUSHI)
                                     ? 0
                                     : str->offset.value);
                            int32 offset_relative_to_sp =
                                func_asm->stack_size + offset_value;
                            str->base.value = sp;
                            str->offset = make_imm(offset_relative_to_sp);
                        }
                    }
                }
            }

            // insert push/pops to save these registers
            if (!needs_save.empty()) {
                // insert push in the start block
                std::sort(needs_save.begin(), needs_save.end());
                auto store = new MI_Push;
                for (auto r : needs_save) {
                    store->operands.push(make_reg(r));
                }
                insert((MI *)store, func_asm->mbs[0]->inst);

                // restore registers at exit

                for (auto bb : func_asm->mbs) {
                    if (bb->last_inst && bb->last_inst->tag == MI_RETURN) {
                        auto restore = new MI_Pop;
                        for (auto r : needs_save) {
                            restore->operands.push(make_reg(r));
                        }
                        insert((MI *)restore, bb->last_inst);
                    }
                }
            }

            // check imm
            bool need_to_legalize_imm = false;
            for (auto mb : func_asm->mbs) {
                for (auto I = mb->inst; I; I = I->next) {
                    auto uses = get_uses(I, func_asm->has_return_value);

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
                            inst_need_legalize =
                                !can_be_imm12(use_of_imm.value);
                            goto done;
                        }
                    }

                    for (auto use : uses) {
                        if (use.tag == IMM) {
                            use_of_imm = use;
                            inst_need_legalize =
                                !can_be_imm_ror(use_of_imm.value);
                            goto done;
                        }
                    }

                done:;

                    need_to_legalize_imm |= inst_need_legalize;

                    if (inst_need_legalize) {
                        printf("%d cannot be imm!\n", use_of_imm.value);

                        // allocate a temp vreg to load the constant from
                        // literal pool
                        auto temp = make_vreg(func_asm->vreg_count++);
                        MI_Load *ldr =
                            emit_load_of_constant(temp, use_of_imm.value);
                        insert(ldr, I);
                        replace_uses(I, use_of_imm, temp);
                    }
                }
            }

            if (need_to_legalize_imm) {
                print_function_asm(func_asm);

                int32 last_size = spilled_stack_size;
                Array<uint8> last_needs_save;
                for (uint8 r : needs_save)
                    last_needs_save.push(r);

                allocate_register(func_asm);

                if (last_size != spilled_stack_size) {
                    exit(91);
                }
                if (last_needs_save.len != needs_save.len) {
                    exit(92);
                } else {
                    for (uint8 r : needs_save) {
                        if (last_needs_save.find(r) == -1) {
                            exit(93);
                        }
                    }
                }
            }

            // done, replace all vreg with actual regs
            for (auto b : func_asm->mbs) {
                for (auto I = func_asm->mbs[b->i]->inst; I; I = I->next) {
                    switch (I->tag) {

                    case MI_CLZ: {
                        color_register(((MI_Clz *)I)->dst);
                        color_register(((MI_Clz *)I)->operand);
                    } break;

                    case MI_MOVE: {
                        color_register(((MI_Move *)I)->dst);
                        color_register(((MI_Move *)I)->src);
                    } break;

                    case MI_BINARY: {
                        color_register(((MI_Binary *)I)->dst);
                        color_register(((MI_Binary *)I)->lhs);
                        color_register(((MI_Binary *)I)->rhs);
                    } break;

                    case MI_COMPARE: {
                        color_register(((MI_Compare *)I)->lhs);
                        color_register(((MI_Compare *)I)->rhs);
                    } break;

                    case MI_LOAD:
                    case MI_STORE: {
                        color_register(((MI_Load *)I)->reg);
                        color_register(((MI_Load *)I)->base);
                        color_register(((MI_Load *)I)->offset);
                    } break;

                    case MI_RETURN:
                    case MI_FUNC_CALL:
                    case MI_PUSH: // we don't push/pop virtual regs, for now
                    case MI_POP:
                    case MI_BRANCH:
                        break;

                    default:
                        exit(65);
                        assert(false);
                    }
                }
            }

            /*
            printf(">>> Allocated registers:\n");
            for(auto it = color.begin(); it != color.end(); it++) {
                print_operand(it->first);
                printf(" <- ");
                print_operand(make_reg(it->second));
                printf("\n");
            }
            printf("\n");
            */
        }
    }
}

void allocate_single_function(Func_Asm *func_asm, bool opt) {
    if (opt && func_asm->vreg_count > 3000) {

        STACK_RA::stack_ra_on_function(func_asm);

    } else {
        spilled_stack_size = 0;
        done_post_work = false;
        already_spilled.clear();
        allocate_register(func_asm);
    }
}

} // namespace RA

void register_allocation(Program_Asm *program_asm, bool opt) {
    printf(">>> Allocating registers...\n\n");
    for (auto f : program_asm->functions) {
        RA::allocate_single_function(f, opt);
    }
}
