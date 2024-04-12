#include "parser.h"

int32 evaluate(Ast_Block *block, Ast_Node *node,
               Array<Ast_Declaration *> globals) {
    switch (node->type) {

    case Ast_Type_Array_Reference: {

        auto arr = (Ast_Array_Reference *)node;

        auto decl = find_declaration(block, arr->array_id, &globals);

        if (!decl->initial_value) {
            return 0;
        }

        auto arr_dims = ((Type_Info_Array *)decl->decl_type)->dimension;

        int32 offset = 0;
        int32 scale = 1;
        for (int i = arr->indices.len - 1; i >= 0; i--) {
            offset += scale * evaluate(block, arr->indices[i], globals);
            scale *= arr_dims[i];
        }

        // @FIXME: this is incredibly slow
        auto init_seq =
            get_init_sequence(arr_dims, (Ast_Init_List *)decl->initial_value);

        // find the element in init sequenece
        int32 curr_index = 0;
        for (auto init_inst : init_seq) {
            if (init_inst.tag == LONG) {
                if (curr_index == offset) {
                    return evaluate(block, init_inst.init_value, globals);
                }
                curr_index++;
            } else {
                if (offset >= curr_index &&
                    offset < curr_index + init_inst.num_of_zeros) {
                    return 0;
                }
                curr_index += init_inst.num_of_zeros;
            }
        }

        exit(10);
        assert(false);
        return 444444444;

    } break;

    case Ast_Type_Identifier: {

        Ast_Identifier *id_node = (Ast_Identifier *)node;

        Ast_Declaration *decl = find_declaration(block, id_node, &globals);

        int32 const_value = evaluate(block, decl->initial_value, globals);
        return const_value;

    } break;

    case Ast_Type_Integer_Literal: {
        auto int_node = (Ast_Integer_Literal *)node;
        return int_node->value;
    } break;

    case Ast_Type_Binary_Op: {
        Ast_Binary_Op *binary_node = (Ast_Binary_Op *)node;

        int32 lhs = evaluate(block, binary_node->lhs, globals);
        int32 rhs = evaluate(block, binary_node->rhs, globals);

        switch (binary_node->op) {
        case BINARY_ADD:
            return lhs + rhs;
        case BINARY_SUBTRACT:
            return lhs - rhs;
        case BINARY_MULTIPLY:
            return lhs * rhs;
        case BINARY_DIVIDE:
            return lhs / rhs;
        case BINARY_MOD:
            return lhs % rhs;
        case BINARY_LOGICAL_AND:
            return lhs && rhs;
        case BINARY_LOGICAL_OR:
            return lhs || rhs;
        case BINARY_LESS_THAN:
            return lhs < rhs;
        case BINARY_GREATER_THAN:
            return lhs > rhs;
        case BINARY_LESS_THAN_OR_EQUAL_TO:
            return lhs <= rhs;
        case BINARY_GREATER_THAN_OR_EQUAL_TO:
            return lhs >= rhs;
        case BINARY_NOT_EQUAL_TO:
            return lhs != rhs;
        case BINARY_EQUAL_TO:
            return lhs == rhs;
        default: {
            exit(11);
            assert(false);
            return 0;
        }
        }

    } break;

    case Ast_Type_Unary_Op: {
        Ast_Unary_Op *unary_node = (Ast_Unary_Op *)node;
        int32 operand = evaluate(block, unary_node->operand, globals);

        switch (unary_node->op) {
        case UNARY_NOT: {
            return !operand;
        } break;
        case UNARY_POSITIVE: {
            return operand;
        } break;
        case UNARY_NEGATIVE: {
            return -operand;
        } break;
        }

    } break;

    default: {
        exit(9);
        report_error(node, "request a complete expression!");
    }
    }

    return -1;
}

Ast_Block *Parser::parse_block(Ast_Block *parent_block,
                               bool32 is_procedure_block,
                               Ast_Block *new_block) {

    if (!new_block)
        new_block = AST_NEW(Ast_Block);

    new_block->parent = parent_block;

    // parse statements within the block
    if (tokenizer.token()->type == '{' || is_procedure_block) {
        tokenizer.expect_and_eat('{');
        new_block->first_statement = parse_statements(new_block);
        tokenizer.expect_and_eat('}');
    } else { // single statement in the block

        new_block->first_statement = parse_statement(new_block);
        // @HACK what is this
        if (tokenizer.token()->type == ';')
            tokenizer.eat();
    }

    return new_block;
}

Type_Info *Parser::parse_basic_type(bool32 return_type) {
    Token_Type token_type = tokenizer.token()->type;
    Type_Info *data_type = NULL;
    switch (token_type) {
    case TOKEN_KEYWORD_INT: {
        data_type = TYPE_NEW(Type_Info_Integer);
    } break;
    case TOKEN_KEYWORD_VOID: {
        if (return_type) {
            data_type = TYPE_NEW(Type_Info_Void);
            break;
        }
    }
    default: {
        report_error(tokenizer.token(), "expected a type, got %d",
                     tokenizer.token()->type);
    }
    }
    tokenizer.eat();
    return data_type;
}

// @CLEANPUP: passing around globals is MESSY!!!!!!!!!!
Type_Info *Parser::parse_array_type(Ast_Block *scope) {

    auto array_type = (Type_Info_Array *)TYPE_NEW(Type_Info_Array);
    array_type->data_type = TYPE_NEW(Type_Info_Integer);

    while (tokenizer.token()->type == '[') {
        tokenizer.eat(); // eats [, arrives at the size of the array
        if (tokenizer.token()->type == ']') {
            array_type->dimension.push(-1);
        } else {
            // @can it be arguments?
            auto const_expr = parse_expr(scope, 1);
            int32 size = evaluate(scope, const_expr, program->globals);
            array_type->dimension.push(size);
        }
        tokenizer.expect_and_eat(']');
    }
    return (Type_Info *)array_type;
}

// parse things like a[1][2]
Ast_Array_Reference *Parser::parse_array_ref(Ast_Block *scope) {
    auto array_id = AST_NEW(Ast_Identifier);
    array_id->name = tokenizer.token()->data.identifier;
    tokenizer.eat();

    auto array_node = AST_NEW(Ast_Array_Reference);
    array_node->array_id = array_id;
    while (tokenizer.token()->type == '[') {
        tokenizer.eat(); // eats [, arrives at the size of the array
        Ast_Node *index = parse_expr(scope, 1);
        array_node->indices.push(index);
        tokenizer.expect_and_eat(']');
    }
    return array_node;
}

Ast_Init_List *Parser::parse_init_list(Ast_Block *scope) {
    auto init = AST_NEW(Ast_Init_List);
    tokenizer.expect_and_eat('{');
    while (1) {
        if (tokenizer.token()->type == '}')
            break;
        if (tokenizer.token()->type == '{') {
            init->list.push((Ast_Node *)parse_init_list(scope));
        } else {
            init->list.push((Ast_Node *)parse_expr(scope, 1));
        }
        if (tokenizer.token()->type == ',')
            tokenizer.eat();
    }
    tokenizer.expect_and_eat('}');

    return init;
}

// prase procedure declaration
// void add(int a, int b) { return a + b; }
Ast_Procedure *Parser::parse_procedure_declaration() {
    auto proc_decl = AST_NEW(Ast_Procedure);

    Type_Info_Procedure *type = new Type_Info_Procedure;
    type->return_type = parse_basic_type(true);

    tokenizer.expect(TOKEN_IDENTIFIER);
    auto proc_id = AST_NEW(Ast_Identifier);
    proc_id->name = tokenizer.token()->data.identifier;
    tokenizer.eat();

    // parse arguments
    tokenizer.expect_and_eat('(');
    while (1) {
        if (tokenizer.token()->type == ')') {
            break;
        } else if (tokenizer.token()->type == ',') {
            tokenizer.eat();
        }

        tokenizer.expect_and_eat(TOKEN_KEYWORD_INT);
        tokenizer.expect(TOKEN_IDENTIFIER);
        auto argument_id = AST_NEW(Ast_Identifier);
        argument_id->name = tokenizer.token()->data.identifier;
        tokenizer.eat();

        if (tokenizer.token()->type == '[') {
            type->argument_types.push(parse_array_type());
        } else {
            type->argument_types.push(TYPE_NEW(Type_Info_Integer));
        }

        type->argument_names.push(argument_id);
    }

    tokenizer.eat(); // eats )

    auto proc_block = AST_NEW(Ast_Block);

    // add argument definition into the procedure block
    for (int arg_i = 0; arg_i < type->argument_types.len; arg_i++) {
        auto arg_decl = AST_NEW(Ast_Declaration);
        arg_decl->arg_index = arg_i;
        arg_decl->id = type->argument_names[arg_i];
        arg_decl->decl_type = type->argument_types[arg_i];
        proc_block->declarations.push(arg_decl);
    }

    parse_block(NULL, true, proc_block);

    // @note: special handling of empty function body
    // insert an empty statement
    if (proc_block->first_statement == NULL) {
        auto empty_stmt = new Ast_Statement;
        empty_stmt->body = NULL;
        proc_block->first_statement = empty_stmt;
    }

    proc_block->belongs_to_procedure = true;

    // @Cleanup
    proc_decl->proc_body = proc_block;
    proc_decl->proc_id = proc_id;
    proc_decl->proc_type = (Type_Info *)type;

    return proc_decl;
}

// parse variable declaration
// int a;
// int a = 5;
// int a[5];
// int a[5][4] = {1};
Ast_Declaration_List *
Parser::parse_declaration(Ast_Block *scope, Array<Ast_Declaration *> *decls) {
    auto decl_list = AST_NEW(Ast_Declaration_List);

    bool is_const = false;
    if (tokenizer.token()->type == TOKEN_KEYWORD_CONST) {
        is_const = true;
        tokenizer.eat();
    }

    tokenizer.expect_and_eat(TOKEN_KEYWORD_INT);

    while (tokenizer.token()->type != ';') {
        auto var_decl = AST_NEW(Ast_Declaration);
        var_decl->is_const = is_const;

        tokenizer.expect(TOKEN_IDENTIFIER);
        auto id_node = AST_NEW(Ast_Identifier);
        id_node->name = tokenizer.token()->data.identifier;
        var_decl->id = id_node;
        tokenizer.eat();

        if (tokenizer.token()->type == '[') {
            var_decl->decl_type = parse_array_type(scope);
        } else {
            var_decl->decl_type = TYPE_NEW(Type_Info_Integer);
        }

        // @Cleanup
        if (scope != NULL) {
            decls = &scope->declarations;
        }

        // @note: disabled for bad O(n^2) performance
        /*
        if(check_for_redeclaration(*decls, var_decl->id->name)) {
            report_error((Ast_Node *)var_decl, "variable '%s' has been already
        declared", var_decl->id->name);
        }
        */

        if (var_decl->is_const)
            tokenizer.expect('=');

        if (tokenizer.token()->type == '=') {
            // initial value
            tokenizer.eat();
            if (tokenizer.token()->type == '{') {
                // initialization list
                var_decl->initial_value = (Ast_Node *)parse_init_list(scope);
            } else {
                var_decl->initial_value = parse_expr(scope, 1);
                if (is_const) {
                    int32 const_val = evaluate(scope, var_decl->initial_value,
                                               program->globals);
                    auto int_node = AST_NEW(Ast_Integer_Literal);
                    int_node->value = const_val;
                    var_decl->initial_value = (Ast_Node *)int_node;
                }
            }
            // @TODO: handle error
        } else if (tokenizer.token()->type == ';' ||
                   tokenizer.token()->type == ',') {
            // variable with no initial value
            var_decl->initial_value = NULL;
        } else {
            report_error((Ast_Node *)var_decl,
                         "expected comma, semicolon or an initial value for "
                         "variable '%s'",
                         var_decl->id->name);
        }
        decl_list->decls.push(var_decl);
        (*decls).push(var_decl);
        if (tokenizer.token()->type == ',')
            tokenizer.eat();
    }

    return decl_list;
}

// foo(1, 2);
Ast_Procedure_Call *Parser::parse_procedure_call(Ast_Block *scope) {
    auto proc_id = AST_NEW(Ast_Identifier);
    proc_id->name = tokenizer.token()->data.identifier;
    if (strcmp(proc_id->name, "starttime") == 0) {
        proc_id->name = "_sysy_starttime";
    }
    if (strcmp(proc_id->name, "stoptime") == 0) {
        proc_id->name = "_sysy_stoptime";
    }
    tokenizer.eat();

    auto call_node = AST_NEW(Ast_Procedure_Call);
    call_node->procedure_id = proc_id;

    tokenizer.expect_and_eat('(');
    while (tokenizer.token()->type != ')') {
        Ast_Node *arg = parse_expr(scope, 1);
        call_node->arguments.push(arg);

        if (tokenizer.token()->type == ',') {
            tokenizer.eat();
        } // if ), don't eat
    }
    tokenizer.expect_and_eat(')');

    return call_node;
}

// @FIXME: it's a giant mistake to handle if/while things
// in parse_atom, which should be dealt in parse_statement?
Ast_Node *Parser::parse_atom(Ast_Block *scope) {

    Ast_Node *result = NULL;

    Token *t = tokenizer.token();
    if (t->type == '(') {
        tokenizer.eat();
        result = parse_expr(scope, 1);
        tokenizer.expect_and_eat(')');
    } else if (t->is_op) { // unary ops

        // turn things like
        // TOKEN_MINUS TOKEN_INTEGER
        // into int literal
        if (t->type == '-' && tokenizer.peek()->type == TOKEN_INTEGER) {
            tokenizer.eat();
            t = tokenizer.token();

            auto int_node = AST_NEW(Ast_Integer_Literal);
            int_node->value =
                -(t->data.int_val); // @NOTE! token->data.int_val is int64 to
                                    // work with INT_MIN
            result = (Ast_Node *)int_node;
            tokenizer.expect_and_eat(TOKEN_INTEGER);

        } else {

            auto unary_node = AST_NEW(Ast_Unary_Op);
            switch (t->type) {
            case '+':
                unary_node->op = UNARY_POSITIVE;
                break;
            case '-':
                unary_node->op = UNARY_NEGATIVE;
                break;
            case '!':
                unary_node->op = UNARY_NOT;
                break;
            default:
                report_error(t, " Having not this operator %s!\n",
                             token_to_desc(t));
            }

            tokenizer.eat();

            // @Fix the precedence
            unary_node->operand = parse_expr(scope, 15);

            result = (Ast_Node *)unary_node;
        }

    } else if (t->type == '{') { // bare block in the wild

        result = (Ast_Node *)parse_block(scope);

    } else if (t->type == TOKEN_END) {
        return 0;
    } else if (t->type == TOKEN_KEYWORD_IF) {

        auto if_node = AST_NEW(Ast_If);
        tokenizer.eat(); // if
        tokenizer.expect_and_eat('(');
        if_node->condition = parse_expr(scope, 1);
        tokenizer.expect_and_eat(')');
        if_node->if_block = parse_block(scope);

        // possibly follows an else statement
        if (tokenizer.token()->type == TOKEN_KEYWORD_ELSE) {
            tokenizer.eat(); // eats else
            if_node->else_block = parse_block(scope);
        }

        result = (Ast_Node *)if_node;

    } else if (t->type == TOKEN_KEYWORD_WHILE) {

        auto while_node = AST_NEW(Ast_While);

        tokenizer.eat(); // while
        tokenizer.expect_and_eat('(');
        while_node->condition = parse_expr(scope, 1);
        tokenizer.expect_and_eat(')');

        bool32 is_parent_inside_loop = inside_loop;
        inside_loop = true;
        while_node->while_block = parse_block(scope);
        inside_loop = is_parent_inside_loop;

        result = (Ast_Node *)while_node;

    } else if (t->type == TOKEN_KEYWORD_RETURN) {

        auto return_node = AST_NEW(Ast_Procedure_Return);
        tokenizer.eat(); // eats return
        return_node->return_value = parse_expr(scope, 1);

        result = (Ast_Node *)return_node;

    } else if (t->type == TOKEN_KEYWORD_CONTINUE) {
        result = (Ast_Node *)AST_NEW(Ast_Continue);
        tokenizer.eat(); // eats continue
    } else if (t->type == TOKEN_KEYWORD_BREAK) {
        result = (Ast_Node *)AST_NEW(Ast_Break);
        tokenizer.eat(); // eats break
    } else if (t->type == TOKEN_KEYWORD_CONST || t->type == TOKEN_KEYWORD_INT) {
        // variable declaration
        auto decl_list = parse_declaration(scope);
        result = (Ast_Node *)decl_list;
    } else if (t->type == TOKEN_IDENTIFIER) {
        // could be procedure calls, variable uses or assignments
        // foo(1, 2)
        // a
        // a = 2

        if (tokenizer.peek()->type == '(') {
            result = (Ast_Node *)parse_procedure_call(scope);
        } else {
            // variable uses or assignments

            /*
            if(!find_declaration(scope, t->data.identifier, &program->globals))
            { report_error(t, "undefined variable '%s'", t->data.identifier);
            }
            */

            if (tokenizer.peek()->type == '[') {
                // array reference
                result = (Ast_Node *)parse_array_ref(scope);
            } else {
                auto id_node = AST_NEW(Ast_Identifier);
                // strcpy_s(id_node->name, MAX_IDENT_LEN, t->data.identifier);
                id_node->name = tokenizer.token()->data.identifier;
                tokenizer.eat();
                result = (Ast_Node *)id_node;
            }
        }
    } else if (t->type == TOKEN_INTEGER) {

        auto int_node = AST_NEW(Ast_Integer_Literal);

        tokenizer.eat();

        int_node->value = t->data.int_val;
        result = (Ast_Node *)int_node;
    }
    return result;
}

Ast_Node *Parser::parse_expr(Ast_Block *scope, int min_prec) {

    Ast_Node *lhs = parse_atom(scope);

    // parse binary operators

    while (1) {
        Token *t = tokenizer.token();

        if (lhs == NULL || !is_expr(lhs) || !t->is_op)
            break;

        Binary_Op_Type op_type;

        bool32 not_a_binary_op = false;

        enum Assocativity { LEFT, RIGHT };
        int prec;
        Assocativity assoc;

        switch (t->type) {
        case '+': {
            op_type = BINARY_ADD;
            prec = 12;
            assoc = LEFT;
        } break;
        case '-': {
            op_type = BINARY_SUBTRACT;
            prec = 12;
            assoc = LEFT;
        } break;
        case '*': {
            op_type = BINARY_MULTIPLY;
            prec = 13;
            assoc = LEFT;
        } break;
        case '/': {
            op_type = BINARY_DIVIDE;
            prec = 13;
            assoc = LEFT;
        } break;
        case '%': {
            op_type = BINARY_MOD;
            prec = 13;
            assoc = LEFT;
        } break;
        case TOKEN_LOGICAL_AND: {
            op_type = BINARY_LOGICAL_AND;
            prec = 9;
            assoc = LEFT;
        } break;
        case TOKEN_LOGICAL_OR: {
            op_type = BINARY_LOGICAL_OR;
            prec = 8;
            assoc = LEFT;
        } break;
        case TOKEN_LESS_THAN: {
            op_type = BINARY_LESS_THAN;
            prec = 11;
            assoc = LEFT;
        } break;
        case TOKEN_GREATER_THAN: {
            op_type = BINARY_GREATER_THAN;
            prec = 11;
            assoc = LEFT;
        } break;
        case TOKEN_LESS_THAN_OR_EQUAL_TO: {
            op_type = BINARY_LESS_THAN_OR_EQUAL_TO;
            prec = 11;
            assoc = LEFT;
        } break;
        case TOKEN_GREATER_THAN_OR_EQUAL_TO: {
            op_type = BINARY_GREATER_THAN_OR_EQUAL_TO;
            prec = 11;
            assoc = LEFT;
        } break;
        case TOKEN_NOT_EQUAL_TO: {
            op_type = BINARY_NOT_EQUAL_TO;
            prec = 10;
            assoc = LEFT;
        } break;
        case TOKEN_EQUAL_TO: {
            op_type = BINARY_EQUAL_TO;
            prec = 10;
            assoc = LEFT;
        } break;

        default: {
            not_a_binary_op = true;
        }
        }

        if (not_a_binary_op || prec < min_prec)
            break;

        auto binary_node = AST_NEW(Ast_Binary_Op);

        tokenizer.eat();
        int next_min_prec = (assoc == LEFT ? prec + 1 : prec);
        Ast_Node *rhs = parse_expr(scope, next_min_prec);
        if (!is_expr(rhs)) {
            report_error(t,
                         "expected an expression on the right hand side of %s",
                         token_to_desc(t));
        }

        binary_node->op = op_type;
        binary_node->lhs = lhs;
        binary_node->rhs = rhs;

        lhs = (Ast_Node *)binary_node;
    }

    // parse declaration or assignment
    if (tokenizer.token()->type == '=') {

        auto assign_node = AST_NEW(Ast_Assignment);

        if (lhs->type != Ast_Type_Identifier &&
            lhs->type != Ast_Type_Array_Reference) {
            report_error((Ast_Node *)assign_node,
                         "expected an identifier or array reference on the "
                         "left of assignment");
        }

        assign_node->lhs = lhs;

        Ast_Identifier *ident;
        if (lhs->type == Ast_Type_Identifier) {
            ident = (Ast_Identifier *)lhs;
        } else {
            ident = ((Ast_Array_Reference *)lhs)->array_id;
        }

        auto decl = find_declaration(scope, ident, &program->globals);

        if (!decl) {
            report_error((Ast_Node *)assign_node, "undefined variable '%s'",
                         ident->name);
        }

        if (decl->is_const) {
            report_error((Ast_Node *)assign_node,
                         "assigning to a constant value '%s'", ident->name);
        }

        tokenizer.eat();

        assign_node->value_to_assign = parse_expr(scope, 1);
        if (!assign_node->value_to_assign) {
            // @TODO: print array ref here
            report_error((Ast_Node *)assign_node,
                         "expected an value to assign for variable '%s'",
                         ident->name);
        }

        lhs = (Ast_Node *)assign_node;
    }

    return lhs;
}

bool32 Parser::is_expr(Ast_Node *node) {
    Ast_Node_Type typ = node->type;
    return (typ == Ast_Type_Identifier || typ == Ast_Type_Array_Reference ||
            typ == Ast_Type_Integer_Literal || typ == Ast_Type_Binary_Op ||
            typ == Ast_Type_Unary_Op || typ == Ast_Type_Procedure_Call ||
            typ == Ast_Type_Statement);
}

Ast_Statement *Parser::parse_statement(Ast_Block *block) {
    auto stm = AST_NEW(Ast_Statement);

    // empty statement
    if (tokenizer.token()->type == ';') {
        stm->body = NULL;
        tokenizer.eat();
        return stm;
    }

    stm->body = parse_expr(block, 1);
    if (!stm->body)
        return NULL;

    // Can only use break and continue inside loops
    if (!inside_loop) {
        if (stm->body->type == Ast_Type_Continue ||
            stm->body->type == Ast_Type_Break) {
            report_error("Can only use break and continue inside a loop");
        }
    }

    // @Cleanup
    // consume the ;
    // not for blocks, ifs and whiles though
    if (stm->body->type != Ast_Type_Block && stm->body->type != Ast_Type_If &&
        stm->body->type != Ast_Type_While) {
        tokenizer.expect_and_eat(';');
    }

    return stm;
}

Ast_Statement *Parser::parse_statements(Ast_Block *block) {

    auto first_statement = parse_statement(block);

    Ast_Statement *cur_stm = first_statement;
    while (1) {
        if (!cur_stm)
            break;
        Ast_Statement *next_stm = parse_statement(block);
        cur_stm->next = next_stm;
        cur_stm = next_stm;
    }

    return first_statement;
}

Program_AST *Parser::parse(const char *filename) {

    tokenizer.load_source_file(filename);
    tokenizer.scan();

    program = new Program_AST;

    // parse procedure declaration and global declaration, e.g.:
    // void foo(int a, int b)
    // int foo = 2;
    // int foo[2] = 3;
    while (1) {
        if (tokenizer.token()->type == TOKEN_END)
            break;
        if (tokenizer.peek(2)->type == '(') {
            auto proc_decl = parse_procedure_declaration();
            // add return statement for void function
            auto proc_type = (Type_Info_Procedure *)proc_decl->proc_type;
            // if (proc_type->return_type->tag == TYPE_VOID) {
            if (true) {
                Ast_Statement *prev = NULL,
                              *stmt = proc_decl->proc_body->first_statement;

                while (stmt) {
                    prev = stmt;
                    stmt = stmt->next;
                }

                if (!prev)
                    continue;

                if (prev->body == NULL ||
                    prev->body->type != Ast_Type_Procedure_Return) {
                    auto ret = AST_NEW(Ast_Procedure_Return);
                    if (proc_type->return_type->tag == TYPE_INTEGER) {
                        auto zero = AST_NEW(Ast_Integer_Literal);
                        zero->value = 0;
                        ret->return_value = (Ast_Node *)zero;
                    }

                    auto ret_stmt = AST_NEW(Ast_Statement);
                    ret_stmt->body = (Ast_Node *)ret;
                    ret_stmt->next = NULL;
                    prev->next = ret_stmt;
                }
            }
            program->procedures.push(proc_decl);
        } else {
            parse_declaration(NULL, &program->globals);
            tokenizer.expect_and_eat(';');
        }
    }

    for (int i = 0; i < program->globals.len; i++) {
        program->globals[i]->is_global = true;
        program->globals[i]->glo_index = i;
    }

    return program;
}

void report_error(Ast_Node *node, const char *format, ...) {
    va_list args;
    va_start(args, format);

    printf("Line %d: ", node->l);
    vprintf(format, args);
    printf("\n");

    va_end(args);
    exit(250);
    assert(false);
    exit(0);
}

Ast_Declaration *check_for_redeclaration(Array<Ast_Declaration *> decls,
                                         const char *name) {
    for (int decl_index = 0; decl_index < decls.len; decl_index++) {
        if (strcmp(decls[decl_index]->id->name, name) == 0) {
            return decls[decl_index];
        }
    }
    return 0;
}

// walk up the tree of blocks to find the declaration
Ast_Declaration *find_declaration(Ast_Block *block, Ast_Identifier *id,
                                  Array<Ast_Declaration *> *globals) {
    Ast_Declaration *decl = NULL;
    while (block) {
        decl = check_for_redeclaration(block->declarations, id->name);
        if (decl) {
            if ((decl->l < id->l) || (decl->l == id->l && decl->c < id->c)) {
                return decl;
            } else {
                decl = NULL;
            }
        }

        block = block->parent;
    }

    if (!decl && globals) {
        decl = check_for_redeclaration(*globals, id->name);
    }

    return decl;
}

Ast_Declaration *find_declaration(Ast_Block *block, const char *name,
                                  Array<Ast_Declaration *> *globals) {
    Ast_Declaration *decl = NULL;
    while (block) {
        decl = check_for_redeclaration(block->declarations, name);
        if (decl) {
            return decl;
        }

        block = block->parent;
    }

    if (!decl && globals) {
        decl = check_for_redeclaration(*globals, name);
    }

    return decl;
}

const char *type_info_to_desc(Type_Info *info) {
    switch (info->tag) {
    case TYPE_VOID: {
        return "void";
    } break;
    case TYPE_INTEGER: {
        return "int";
    } break;
    case TYPE_PROCEDURE: {
        return "procedure";
    } break;
    default: {
        return "(unknown)";
    } break;
    }
}
