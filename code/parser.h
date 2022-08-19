#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "general.h"
#include "tokenizer.h"

#include <string.h> // strcpy

enum Type_Info_Tag { TYPE_VOID, TYPE_INTEGER, TYPE_PROCEDURE, TYPE_ARRAY };

struct Type_Info {
    Type_Info_Tag tag;
};

struct Type_Info_Void {
    Type_Info_Tag tag = TYPE_VOID;
};

struct Type_Info_Integer {
    Type_Info_Tag tag = TYPE_INTEGER;
};

struct Type_Info_Procedure {
    Type_Info_Tag tag = TYPE_PROCEDURE;

    Type_Info *return_type;

    Array<Ast_Identifier *> argument_names;
    Array<Type_Info *> argument_types;
};

struct Type_Info_Array {
    Type_Info_Tag tag = TYPE_ARRAY;

    Type_Info *data_type;
    Array<int32> dimension;
};

const char *type_info_to_desc(Type_Info *info);

static Type_Info *temp_type;
#define TYPE_NEW(TYPE_INFO)                                                    \
    ((temp_type = (Type_Info *)(new TYPE_INFO)), (Type_Info *)temp_type)

struct Parser {
    Tokenizer tokenizer;
    Program_AST *program;

    bool32 inside_loop = false;

    Ast_Block *parse_block(Ast_Block *parent_block,
                           bool32 is_procedure_block = false,
                           Ast_Block *new_block = NULL);
    Type_Info *parse_basic_type(bool32 return_type);
    Type_Info *parse_type(bool32 return_type = false);
    Type_Info *parse_array_type(Ast_Block *scope = NULL);
    Ast_Array_Reference *parse_array_ref(Ast_Block *scope);
    Ast_Procedure *parse_procedure_declaration();
    Ast_Declaration_List *
    parse_declaration(Ast_Block *scope = NULL,
                      Array<Ast_Declaration *> *decls = NULL);
    Ast_Procedure_Call *parse_procedure_call(Ast_Block *scope);
    Ast_Init_List *parse_init_list(Ast_Block *scope);
    Ast_Node *parse_atom(Ast_Block *scope);
    Ast_Node *parse_expr(Ast_Block *scope, int min_prec = 1);
    bool32 is_expr(Ast_Node *node);
    Ast_Statement *parse_statement(Ast_Block *block);
    Ast_Statement *parse_statements(Ast_Block *block);
    Program_AST *parse(const char *filename);
};

void report_error(Ast_Node *node, const char *format, ...);

// check if the given block contains such name
Ast_Declaration *check_for_redeclaration(Array<Ast_Declaration *> decls,
                                         const char *name);

// walk up the tree of blocks to find the declaration
Ast_Declaration *find_declaration(Ast_Block *block, Ast_Identifier *id,
                                  Array<Ast_Declaration *> *globals);
Ast_Declaration *find_declaration(Ast_Block *block, const char *name,
                                  Array<Ast_Declaration *> *globals = NULL);

int32 evaluate(Ast_Block *block, Ast_Node *node,
               Array<Ast_Declaration *> globals);

#endif
