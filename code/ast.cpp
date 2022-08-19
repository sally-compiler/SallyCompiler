#include "ast.h"

bool is_literal_0(Ast_Node *n) {
    if (n->type != Ast_Type_Integer_Literal)
        return false;
    auto int_node = (Ast_Integer_Literal *)n;
    return (int_node->value == 0);
}

// @Shit code
// But it works
// FIXME: this function could return an array of elements to be inited
// don't have to take care of collpasing zeros....
// Drawback is highly space ineffecient

Array<Init_Inst> get_init_sequence(Array<int32> dims,
                                   Ast_Init_List *init_list) {
    Array<Init_Inst> init_seq;

    // for one dimension array, also for the top level of init list,
    // there are dimension[0]*dimension[1]*... elements need to be initialized
    uint64 one_d = 1;
    int DIM = dims.len;
    for (int i = 0; i < DIM; i++)
        one_d *= dims[i];

    Array<uint64> need_to_init;
    Array<uint64> fill_to;
    Array<Ast_Init_List *> init_lists;
    Array<uint64> list_indices;
    need_to_init.push(one_d);
    fill_to.push(one_d);
    init_lists.push(init_list);
    list_indices.push(0);

    int index = 0;

    while (1) {
        Ast_Init_List *init = init_lists.back();
        init_lists.pop();
        int prev_index = list_indices.back();
        list_indices.pop();
        size_t len = init->list.len;
        uint64 i;
        for (i = prev_index; i < len; i++) {
            if (init->list[i]->type == Ast_Type_Init_List) {
                uint64 level = init_lists.len;
                list_indices.push(i + 1);
                list_indices.push(0);
                init_lists.push(init);
                init_lists.push((Ast_Init_List *)init->list[i]);
                uint64 last_need = need_to_init.back();
                uint64 div = dims[level];
                need_to_init.push(last_need / div);
                fill_to.push(index + last_need / div);
                break;
            } else {
                Ast_Node *item_expr = init->list[i];

                // @TODO: only collapse when zeros are big
                // because memset is expensive?
                // collapse into previous zeros if item_expr is 0
                if (is_literal_0(item_expr)) {
                    if (!init_seq.empty()) {
                        if (init_seq.back().tag == ZEROS) {
                            init_seq[init_seq.len - 1].num_of_zeros++;
                            goto done;
                        } else {
                            auto prev_inst = init_seq.back();
                            if (is_literal_0(prev_inst.init_value)) {
                                init_seq.pop();
                                init_seq.push(Init_Inst::make_zeros(2));
                                goto done;
                            }
                        }
                    }
                }

                init_seq.push(Init_Inst::make_long(item_expr));

            done:;
                index++;
            }
        }

        if (i == len) { // reaches the end of init list
            uint64 need = fill_to.back();
            fill_to.pop();
            need_to_init.pop();
            if (index + 1 == need) {
                auto zero = new Ast_Integer_Literal;
                zero->value = 0;
                init_seq.push(Init_Inst::make_long((Ast_Node *)zero));
            } else if (index != need) {
                init_seq.push(Init_Inst::make_zeros(need - index));
            }
            index = need;
            if (init_lists.len == 0)
                break;
        }
    }

    return init_seq;
}

char *print_ast_for_ir(Ast_Node *n, bool with_line_number) {
    String_Builder s;
    print_ast(&s, n, true);

    if (with_line_number) {
        s.append(" (%d)", n->l);
    }
    s.add_terminator();
    return s.c_str();
}

// @Cleanup: that cfg viewer flag is exceptionally annoying
// @FIXME: \< in terminals
void print_ast(String_Builder *s, Ast_Node *n, bool is_cfg_viewer) {
    const char *le = is_cfg_viewer ? "\\<=" : "<=";
    const char *ge = is_cfg_viewer ? "\\>=" : ">=";
    const char *lt = is_cfg_viewer ? "\\<" : "<";
    const char *gt = is_cfg_viewer ? "\\>" : ">";
    const char *logical_or = is_cfg_viewer ? "\\|\\|" : "||";
    switch (n->type) {
    case Ast_Type_Integer_Literal: {
        auto int_node = (Ast_Integer_Literal *)n;
        s->append("%d", int_node->value);
    } break;

    case Ast_Type_Init_List: {
        auto init = (Ast_Init_List *)n;
        s->append("\\{");
        for (int i = 0; i < init->list.len; i++) {
            auto item = init->list[i];
            print_ast(s, init->list[i], is_cfg_viewer);
            if (i != init->list.len - 1)
                s->append(", ");
        }
        s->append("\\}");
    } break;

    case Ast_Type_Identifier: {
        auto id_node = (Ast_Identifier *)n;
        s->append("%s", id_node->name);
    } break;

    case Ast_Type_Assignment: {
        auto assign_node = (Ast_Assignment *)n;
        print_ast(s, assign_node->lhs, is_cfg_viewer);
        s->append(" = ");
        print_ast(s, assign_node->value_to_assign, is_cfg_viewer);
    } break;

    // @TODO: declartion for arrays?
    case Ast_Type_Declaration: {
        auto decl_node = (Ast_Declaration *)n;
        if (decl_node->arg_index != -1) {
            s->append("arg: ");
        } else {
            s->append("local: ");
        }
        s->append("%s", decl_node->id->name);
        if (decl_node->initial_value) {
            s->append(" = ");
            print_ast(s, decl_node->initial_value, is_cfg_viewer);
        }
    } break;

    // @TODO: Precedence........
    case Ast_Type_Binary_Op: {
        auto bi_node = (Ast_Binary_Op *)n;

        print_ast(s, bi_node->lhs, is_cfg_viewer);
        s->append(" ");

        switch (bi_node->op) {
        case BINARY_ADD: {
            s->append("+");
        } break;
        case BINARY_SUBTRACT: {
            s->append("-");
        } break;
        case BINARY_MULTIPLY: {
            s->append("*");
        } break;
        case BINARY_DIVIDE: {
            s->append("/");
        } break;
        case BINARY_MOD: {
            s->append("%%");
        } break; // NOTE that we are using sprintf... sigh...
        case BINARY_LESS_THAN: {
            s->append(lt);
        } break;
        case BINARY_GREATER_THAN: {
            s->append(gt);
        } break;
        case BINARY_LESS_THAN_OR_EQUAL_TO: {
            s->append(le);
        } break;
        case BINARY_GREATER_THAN_OR_EQUAL_TO: {
            s->append(ge);
        } break;
        case BINARY_NOT_EQUAL_TO: {
            s->append("!=");
        } break;
        case BINARY_EQUAL_TO: {
            s->append("==");
        } break;
        case BINARY_LOGICAL_OR: {
            s->append(logical_or);
        } break;
        case BINARY_LOGICAL_AND: {
            s->append("&&");
        } break;
        case BINARY_LSL: {
            s->append("<<");
        } break;
        case BINARY_LSR: {
            s->append(">>");
        } break;
        case BINARY_ASL: {
            s->append("<<");
        } break;
        case BINARY_ASR: {
            s->append(">>");
        } break;
        case BINARY_RSB: // aha?
        default: {
            assert(false && "printing unknown AST binary op");
        }
        }

        s->append(" ");
        print_ast(s, bi_node->rhs, is_cfg_viewer);

    } break;

    case Ast_Type_Unary_Op: {
        auto unary_node = (Ast_Unary_Op *)n;

        switch (unary_node->op) {
        case UNARY_NEGATIVE: {
            s->append("-");
        } break;
        case UNARY_POSITIVE: {
        } break;
        case UNARY_NOT: {
            s->append("!");
        } break;
        default: {
            assert(false && "printing unknown AST unary op");
        }
        }
        print_ast(s, unary_node->operand, is_cfg_viewer);

    } break;

    case Ast_Type_Continue: {
        s->append("continue");
    } break;

    case Ast_Type_Break: {
        s->append("break");
    } break;

    case Ast_Type_Procedure_Return: {
        auto ret_node = (Ast_Procedure_Return *)n;
        s->append("return");
        if (ret_node->return_value) {
            s->append(" ");
            print_ast(s, ret_node->return_value, is_cfg_viewer);
        }
    } break;

    case Ast_Type_Array_Reference: {
        auto arr_node = (Ast_Array_Reference *)n;
        print_ast(s, (Ast_Node *)arr_node->array_id, is_cfg_viewer);

        for (Ast_Node *index : arr_node->indices) {
            s->append("[");
            print_ast(s, index, is_cfg_viewer);
            s->append("]");
        }

    } break;

    case Ast_Type_Procedure_Call: {
        auto call_node = (Ast_Procedure_Call *)n;
        print_ast(s, (Ast_Node *)call_node->procedure_id, is_cfg_viewer);

        s->append("(");
        for (int i = 0; i < call_node->arguments.len; i++) {
            print_ast(s, call_node->arguments[i], is_cfg_viewer);
            if (i != call_node->arguments.len - 1) {
                s->append(", ");
            }
        }
        s->append(")");

    } break;

    default: {
        assert(false && "can't print this type of AST node");
    }
    }
}
