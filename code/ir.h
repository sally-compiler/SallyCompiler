#ifndef IR_H
#define IR_H

// for IR printing
#include <sstream>
#include <string>

#include "ast.h"
#include "general.h"
#include "parser.h"

struct Basic_Block;
struct Value;
struct Phi;

typedef Pair<Basic_Block *, Basic_Block *> Entry_Exit_Block;

typedef Pair<Basic_Block *, Basic_Block *> Flow_Edge;

// defs information is in Ast_Declaration
struct Loop;
struct Argument;
struct Memory_Def;

// @Cleanup, oh my
typedef Array<Pair<Ast_Declaration *, Value *>> Version_Map;

// @cleanup
typedef Map<Value *, Map<Basic_Block *, Map<Ast_Declaration *, bool>>> Trash;

struct Procedure_IR {
    int values_count = 0;
    int args_count = 0;
    bool has_return_value = true;
    bool has_side_effect = true;
    bool is_builtin = false;

    Ast_Declaration *whole_memory = NULL;
    Map<Basic_Block *, Version_Map> bb_vermap;
    Trash trash;

    const char *name;
    Basic_Block *start_block;
    Basic_Block *exit_block;

    Array<Basic_Block *> blocks;
    Array<Argument *> arguments;

    Array<Procedure_IR *> callee, caller;

    Set<Flow_Edge> edges;
    Array<Loop *> loops;
};

struct Program_IR {
    Array<Ast_Declaration *> globals;
    Array<Procedure_IR *> procedures;
};

void connect_CFG(Basic_Block *from, Basic_Block *to,
                 bool insert_br_to_from = false);
void push_block(Procedure_IR *f, Basic_Block *bb);

struct Basic_Block {
    int32 index_in_procedure = -1;

    // a handy integer for optmization purposes
    // it's used, for exmaple, by the simplify_cfg pass,
    // to tell which basic block is dead (mark == BB_DEAD)
    int32 mark = 0;

    // mark the condition block
    bool is_condition_block = false;
    bool is_loop_header = false;
    bool32 sealed =
        true; // most blocks are sealed at start, except for loop headers

    // for block placement
    // try to place together blocks that's in the same loop!
    int belongs_to_loop = -1;

    Array<Pair<Ast_Declaration *, Value *>>
        incomplete_phis; // @Cleanup, only for IR emitter
    Array<Value *> insts;
    Array<Basic_Block *> preds;
    Array<Basic_Block *> succs;

    /*
     * Dominator infomation
     * which can be computed by running the compute_dominator pass
     */
    Array<Basic_Block *> dominators;
    Basic_Block *imm_dom = NULL; // immediately dominated by
    int32 dom_depth = -1;

    /* Loop info */
    int32 loop_depth = 0;
    Loop *loop = NULL;

    Basic_Block(Procedure_IR *f = NULL, Basic_Block *parent_bb = NULL) {
        if (f)
            push_block(f, this);
        if (parent_bb)
            connect_CFG(parent_bb, this, true);
    }

    void remove_dead_values();
};

enum Branch_Condition {
    NO_CONDITION = 0,

    LESS_THAN = 1,
    GREATER_THAN = 2,
    NOT_EQUAL = 3,

    GREATER_THAN_OR_EQUAL = 4,
    LESS_THAN_OR_EQUAL = 5,
    EQUAL = 6
};

struct Use {
    Use *prev = NULL, *next = NULL;
    Value *v;
    Value *user;
    bool dead = false;

    // @Cleanup

    void remove();
    void mark_as_dead() { dead = true; }
};

enum Value_Type {
    UNDEF,
    CONSTANT,
    ARGUMENT,
    GLOBAL,
    PHI,
    INST_BINARY,
    INST_UNARY,
    INST_RETURN,
    INST_BRANCH,
    INST_DIRECT_BRANCH,
    MEMORY_READ,
    MEMORY_WRITE,
    MEMORY_DEF,
    MEMORY_PHI,
    ALLOCA,
    FUNCTION_CALL
};

Array<Use *> get_operands(Value *v);

struct Value {
    Value_Type type;
    Use *use = NULL; // linked list
    Basic_Block *b;  // belongs to block
    int n = -1;      // nubmering
    char *comment = NULL;

    bool invariant = false; // for loop invariant

    /* For optimizations like GVN and GCM */
    bool visited = false;
    bool live = true;

    Value(Value_Type type) : type(type) {}

    // replace a value, and replace the old_value in bb with new_value
    void replace_inplace(Value *new_value) {
        assert(b);
        replace_all_uses_with(new_value);

        int p = b->insts.find(this);
        new_value->b = b;
        new_value->n = n;
        new_value->comment = comment;
        b->insts[p] = new_value;

        b = NULL;
        drop_uses_of_operands();
    }

    // replace a value, delete the old value from bb
    void replace_and_remove(Value *new_value, bool mark = false) {
        replace_all_uses_with(new_value);
        if (mark) {
            live = false;
        } else {
            b->insts.remove(this);
        }
        b = NULL;
        drop_uses_of_operands();
    }

    void drop_uses_of_operands() {
        for (auto u : get_operands(this))
            u->remove();
    }

    void replace_all_uses_with(Value *new_value) {
        for (auto u = use; u;) {
            u->v = new_value;
            auto nxt = u->next;
            new_value->add_new_use(u);
            u = nxt;
        }
        use = NULL;
    }

    void clean_dead_uses() {
        for (auto u = use; u;) {
            if (u->dead) {
                auto new_u = u->next;
                u->remove();
                u = new_u;
            } else {
                u = u->next;
            }
        }
    }

    void mark_remove_from_bb() {
        live = false;
        b = NULL;
    }

    void add_new_use(Use *u) {
        u->prev = NULL; // u might come from other use list, need to clean
                        // previous value
        if (use)
            use->prev = u;
        u->next = use;
        use = u;
    }

    ~Value() {
        for (auto u : get_operands(this)) {
            u->remove();
        }
        for (auto u = use; u; u = u->next) {
            u->remove();
        }
    }

    template <typename VT> VT *as() {
        return (type == VT::TYPE) ? static_cast<VT *>(this) : NULL;
    }
};

struct Constant : Value {
    static const Value_Type TYPE = CONSTANT;
    int32 value;
    Constant(int32 v) : Value(CONSTANT), value(v) {}
};

struct Argument : Value {
    static const Value_Type TYPE = ARGUMENT;
    int32 arg_index = -1;
    Ast_Declaration *decl = NULL;
    Argument() : Value(ARGUMENT) {}
};

struct Memory_Def : Value {
    static const Value_Type TYPE = MEMORY_DEF;

    bool is_initial_version = false;
    Use *clobbers = NULL; // clobbers which memory version?
    Use *cause = NULL;    // who is causing the def?
    Ast_Declaration *decl = NULL;

    Memory_Def() : Value(MEMORY_DEF) {}
};

struct Global : Value {
    static const Value_Type TYPE = GLOBAL;
    const char *name;
    Ast_Declaration *decl;
    Global() : Value(GLOBAL) {}
};

struct Phi : Value {
    static const Value_Type TYPE = PHI;
    Array<Use *> operands;
    Array<Basic_Block *> sources; // where operands come from
    // @TODO: one sources per basic block? (no need for each phi)

    Ast_Declaration *decl;

    Phi(Basic_Block *belongs_to) : Value(PHI) { b = belongs_to; }
};

// @note: memory phis can be used as if they are normal phis
struct Memory_Phi : Value {
    static const Value_Type TYPE = MEMORY_PHI;
    Array<Use *> operands;        // can only be Memory_Def
    Array<Basic_Block *> sources; // where operands come from

    // Ast_Declaration *decl; @note: what is this?

    Memory_Phi(Basic_Block *belongs_to) : Value(MEMORY_PHI) { b = belongs_to; }
};

struct Instruction_Binary : Value {
    static const Value_Type TYPE = INST_BINARY;
    Binary_Op_Type op_type; // defined in ast.h
    Use *lhs, *rhs;

    Instruction_Binary() : Value(INST_BINARY) {}
};

struct Instruction_Unary : Value {
    static const Value_Type TYPE = INST_UNARY;
    Unary_Op_Type op_type; // defined in ast.h
    Use *oprend;

    Instruction_Unary() : Value(INST_UNARY) {}
};

struct Instruction_Return : Value {
    static const Value_Type TYPE = INST_RETURN;
    Use *return_value = NULL;

    Instruction_Return() : Value(INST_RETURN) {}
};

struct Instruction_Branch : Value {
    static const Value_Type TYPE = INST_BRANCH;
    Use *cond;
    Basic_Block *true_target;
    Basic_Block *false_target = NULL;

    Instruction_Branch() : Value(INST_BRANCH) {}
};

struct Instruction_Direct_Branch : Value {
    static const Value_Type TYPE = INST_DIRECT_BRANCH;
    Basic_Block *target;

    Instruction_Direct_Branch() : Value(INST_DIRECT_BRANCH) {}
};

struct Memory_Read : Value {
    static const Value_Type TYPE = MEMORY_READ;

    Ast_Declaration *decl = NULL;
    Use *base = NULL; // @Unsigned?
    Use *offset = NULL;
    Use *mem_ver =
        NULL; // which memory version is reading from? can only be memory def

    /* @note
     * bake_constant pass need to know if a Memory_Read
     * is reading an const global
     *
     * so we record the who is generating this mem read
     * can be an global identifier or an array reference of global
     */
    Ast_Node *source;

    Memory_Read() : Value(MEMORY_READ) {}
};

// Memory[base+offset] = value_to_write
// write 4 byte at a time
struct Memory_Write : Value {
    static const Value_Type TYPE = MEMORY_WRITE;
    Ast_Declaration *decl = NULL;
    Use *base = NULL; // @Unsigned?
    Use *offset = NULL;
    Use *value_to_write = NULL;

    Memory_Write() : Value(MEMORY_WRITE) {}
};

/* Allocate a constant size of bytes on the stack */
/* Returns the starting pointer */
struct Alloca : Value {
    static const Value_Type TYPE = ALLOCA;
    Use *size; // can only be a constant!
    Ast_Declaration *decl = NULL;

    Alloca() : Value(ALLOCA) {}
};

struct Function_Call : Value {
    static const Value_Type TYPE = FUNCTION_CALL;
    const char *name;
    Procedure_IR *f = NULL; // who is being called?
    Procedure_IR *caller = NULL;
    bool32 has_return_value;
    Array<Use *> arguments;

    Function_Call() : Value(FUNCTION_CALL) {}
};

/* Utility Functions */

Value *try_remove_trivial_phi(Phi *phi, Ast_Declaration *variable = NULL);
void remove_trivial_phi(Procedure_IR *f);
void put_phis_to_start(Basic_Block *bb);

void renumber_blocks(Procedure_IR *f);
void renumber_values(Procedure_IR *f);
void print_use_list(Procedure_IR *f);

bool is_block_dead(Procedure_IR *func_IR, Basic_Block *bb);

void insert_br(Basic_Block *from, Basic_Block *to);
void compute_CFG(Procedure_IR *IR);

Use *new_use(Value *value, Value *user);
void destroy_use(Use *use);

Branch_Condition binary_op_to_branch_cond(Binary_Op_Type op_type);
Branch_Condition invert_branch_cond(Branch_Condition cond);
const char *get_branch_suffix(Branch_Condition condition);

std::string print_basic_block(Basic_Block *bb, bool is_cfg_viewer = false);
std::string print_comments(Basic_Block *bb);
std::string print_procedure_IR(Procedure_IR *proc);

bool32 has_return(Basic_Block *bb);
bool32 has_branch(Basic_Block *bb);

Procedure_IR *emit_func_IR(Program_AST *program_ast, Ast_Procedure *proc_ast);
Program_IR *emit_IR(Program_AST *program_ast);

int32 find_func_index(Program_IR *prog, const char *func_name);

bool is_relational_binary_op(Binary_Op_Type op);

#endif
