#ifndef AST_H
#define AST_H

#include "array.h"
#include "general.h"
#include <string.h> // strcmp

struct Type_Info;

enum Ast_Node_Type {
    Ast_Type_Statement,
    Ast_Type_If,
    Ast_Type_While,
    Ast_Type_Block,
    Ast_Type_Declaration,
    Ast_Type_Declaration_List,
    Ast_Type_Assignment,
    Ast_Type_Procedure,
    Ast_Type_Procedure_Call,
    Ast_Type_Procedure_Return,
    Ast_Type_Init_List,
    Ast_Type_Identifier,
    Ast_Type_Integer_Literal,
    Ast_Type_Binary_Op,
    Ast_Type_Unary_Op,
    Ast_Type_Array_Reference,
    Ast_Type_Continue,
    Ast_Type_Break
};

struct Ast_Node {
    Ast_Node_Type type;

    // line and column number to help with debugging
    int l, c;
};

// @TODO: Reduce random heap allocations.
struct Ast_Statement {
    Ast_Node_Type type = Ast_Type_Statement;
    int l, c;
    Ast_Statement *next = 0;
    Ast_Node *body = 0;
};

struct Ast_Identifier {
    Ast_Node_Type type = Ast_Type_Identifier;
    int l, c;
    const char *name;
};

struct Basic_Block;
struct Value;
struct Ast_Declaration {
    Ast_Node_Type type = Ast_Type_Declaration;
    int l, c;

    Ast_Identifier *id;
    Ast_Node *initial_value = NULL;
    Type_Info *decl_type = NULL;
    int32 arg_index = -1;
    int32 glo_index = -1;
    bool32 is_const = false;
    bool32 is_global = false;

    Map<Basic_Block *, Value *> value_map;
};

struct Ast_Declaration_List {
    Ast_Node_Type type = Ast_Type_Declaration_List;
    int l, c;

    Array<Ast_Declaration *> decls;
};

struct Ast_Assignment {
    Ast_Node_Type type = Ast_Type_Assignment;
    int l, c;
    Ast_Node *lhs; // could be Ast_Identifier or Ast_Array_Refernece
    Ast_Node *value_to_assign = 0;
};

struct Ast_Procedure_Call {
    Ast_Node_Type type = Ast_Type_Procedure_Call;
    int l, c;
    Ast_Identifier *procedure_id = 0;
    Array<Ast_Node *> arguments;
};

struct Ast_Array_Reference {
    Ast_Node_Type type = Ast_Type_Array_Reference;
    int l, c;
    Ast_Identifier *array_id;
    Array<Ast_Node *> indices;
};

struct Ast_Init_List {
    Ast_Node_Type type = Ast_Type_Init_List;
    int l, c;
    Array<Ast_Node *> list; // items in init list could be any expression
};

struct Ast_Procedure_Return {
    Ast_Node_Type type = Ast_Type_Procedure_Return;
    int l, c;
    Ast_Node *return_value = 0; // an "expression"
};

struct Ast_Block {
    Ast_Node_Type type = Ast_Type_Block;
    int l, c;
    Ast_Block *parent = 0;
    bool32 belongs_to_procedure = false;

    Ast_Statement *first_statement = 0;
    Array<Ast_Declaration *> declarations;
};

struct Ast_If {
    Ast_Node_Type type = Ast_Type_If;
    int l, c;

    Ast_Node *condition = 0;
    Ast_Block *if_block = 0;
    Ast_Block *else_block = 0;
};

struct Ast_While {
    Ast_Node_Type type = Ast_Type_While;
    int l, c;

    Ast_Node *condition = 0;
    Ast_Block *while_block = 0;
};

struct Ast_Integer_Literal {
    Ast_Node_Type type = Ast_Type_Integer_Literal;
    int l, c;

    // initialize to zero by default. @Check?
    int64 value = 0;
};

struct Ast_Continue {
    Ast_Node_Type type = Ast_Type_Continue;
    int l, c;
};

struct Ast_Break {
    Ast_Node_Type type = Ast_Type_Break;
    int l, c;
};

enum Binary_Op_Type {
    BINARY_ADD,
    BINARY_SUBTRACT,
    BINARY_MULTIPLY,
    BINARY_SMMUL,
    BINARY_DIVIDE,
    BINARY_MOD,

    /* ASM BINARY INSTRUCTIONS */
    BINARY_LSL,
    BINARY_LSR,
    BINARY_ASL,
    BINARY_ASR,
    BINARY_RSB,         // reverse sub for machine instruction
    BINARY_BITWISE_AND, // bitwise or
    BINARY_BITWISE_OR,  // bitwise or
    BINARY_BIC,

    BINARY_LOGICAL_AND,
    BINARY_LOGICAL_OR,
    BINARY_NOT_EQUAL_TO,
    BINARY_EQUAL_TO,
    BINARY_LESS_THAN_OR_EQUAL_TO,
    BINARY_GREATER_THAN_OR_EQUAL_TO,
    BINARY_LESS_THAN,
    BINARY_GREATER_THAN
};
struct Ast_Binary_Op {
    Ast_Node_Type type = Ast_Type_Binary_Op;
    int l, c;
    Binary_Op_Type op;
    Ast_Node *lhs, *rhs;
};

enum Unary_Op_Type { UNARY_POSITIVE, UNARY_NEGATIVE, UNARY_NOT };

struct Ast_Unary_Op {
    Ast_Node_Type type = Ast_Type_Unary_Op;
    int l, c;
    Unary_Op_Type op;
    Ast_Node *operand;
};

struct Ast_Procedure {
    Ast_Node_Type type = Ast_Type_Procedure;
    int l, c;

    Ast_Identifier *proc_id;
    Ast_Block *proc_body;
    Type_Info *proc_type;
};

struct Program_AST {
    Array<Ast_Procedure *> procedures;
    Array<Ast_Declaration *> globals;
};

char *print_ast_for_ir(Ast_Node *n, bool with_line_number = true);
void print_ast(String_Builder *s, Ast_Node *n, bool is_cfg_viewer = false);

enum Init_Tag {
    LONG, // init a single 32 bit value
    ZEROS // init a sequence of zeros
};

struct Init_Inst {
    Init_Tag tag;
    uint32 num_of_zeros; // for tag == ZEROS, value is the number zeros to be
                         // initialized
    Ast_Node *init_value =
        NULL; // for tag == LONG, init_value is the AST node to be init

    static Init_Inst make_long(Ast_Node *expr) {
        Init_Inst inst;
        inst.tag = LONG;
        inst.init_value = expr;
        return inst;
    }

    static Init_Inst make_zeros(uint32 number_of_zeros) {
        Init_Inst inst;
        inst.tag = ZEROS;
        inst.num_of_zeros = number_of_zeros;
        return inst;
    }
};

Array<Init_Inst> get_init_sequence(Array<int32> dims, Ast_Init_List *init_list);
bool is_literal_0(Ast_Node *n);

static Ast_Node *temp_node;
#define AST_NEW(AST_NODE)                                                      \
    ((temp_node = (Ast_Node *)(new AST_NODE)),                                 \
     temp_node->l = tokenizer.token()->l, temp_node->c = tokenizer.token()->c, \
     (AST_NODE *)temp_node)

#endif
