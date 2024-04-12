#include "ir.h"

typedef Pair<Basic_Block *, Instruction_Branch *> Block_And_Branch;
typedef Pair<Basic_Block *, Ast_Node *> Block_And_Value;

void Basic_Block::remove_dead_values() {
    int i = 0, j = 0;
    for (; i < insts.len; i++) {
        if (insts[i]->live) {
            insts[j++] = insts[i];
        }
    }
    insts.len = j;
}

void push_block(Procedure_IR *f, Basic_Block *bb) {
    bb->index_in_procedure = f->blocks.len;
    f->blocks.push(bb);
}

void Use::remove() {
    if (prev == NULL && next == NULL) {
        v->use = NULL;
    } else if (prev == NULL && next != NULL) {
        v->use = next;
        next->prev = NULL;
    } else if (prev != NULL && next == NULL) {
        prev->next = NULL;
    } else if (prev != NULL && next != NULL) {
        prev->next = next;
        next->prev = prev;
    }
    // delete this;
}

Phi *has_phi(Basic_Block *bb) {
    bool first = bb->insts.len != 0 && bb->insts[0]->type == PHI;
    if (first)
        return (Phi *)bb->insts[0];

    for (auto v : bb->insts) {
        if (v->type == PHI)
            return (Phi *)v;
    }

    return NULL;
}

bool is_block_dead(Procedure_IR *f, Basic_Block *bb) {
    if (bb == f->start_block || bb == f->exit_block) {
        return false;
    }

    for (auto pred : bb->preds) {
        Flow_Edge edge(pred, bb);

        if (f->edges.find(edge) != f->edges.end()) {
            return false;
        }
    }

    return true;
}

#define CONNECT(a, b) connect_CFG((a), (b)), IR->edges.emplace((a), (b))
void compute_CFG(Procedure_IR *IR) {
    // clear previous CFG edges
    IR->edges.clear();
    for (auto bb : IR->blocks) {
        bb->preds.release();
        bb->succs.release();
    }

    // handle phi edges
    for (auto bb : IR->blocks) {
        if (auto phi = has_phi(bb)) {
            for (auto src : phi->sources) {
                CONNECT(src, bb);
            }
        }
    }

    // handle other edges
    for (auto bb : IR->blocks) {
        if (bb->insts.len != 0) {
            auto last_inst = bb->insts[bb->insts.len - 1];
            if (last_inst->type == INST_BRANCH) {
                auto br = (Instruction_Branch *)last_inst;
                if (!has_phi(br->true_target))
                    CONNECT(bb, br->true_target);
                if (!has_phi(br->false_target))
                    CONNECT(bb, br->false_target);
            } else if (last_inst->type == INST_DIRECT_BRANCH) {
                auto br = (Instruction_Direct_Branch *)last_inst;
                if (!has_phi(br->target))
                    CONNECT(bb, br->target);
            } else if (last_inst->type == INST_RETURN) {
                if (!has_phi(IR->exit_block))
                    CONNECT(bb, IR->exit_block);
            }
        }
    }
}
#undef CONNECT

struct IR_Generator {
    uint32 curr_value_index = 0;
    Procedure_IR *f;
    Program_AST *program_ast = NULL; // @Cleanup
    Basic_Block *bb = NULL;          // current bb

    Array<Basic_Block *> while_headers;
    Array<Basic_Block *> while_ends;
    Array<Basic_Block *> unsealed_blocks;

    IR_Generator(Program_AST *program_ast) : program_ast(program_ast) {}

    uint32 insert(Basic_Block *basic_block, Value *v,
                  bool insert_to_first = false);

    // IR emitting
    Procedure_IR *emit_procedure(Ast_Procedure *proc_ast);
    Value *emit_value(Ast_Node *node, Ast_Block *ast_block);
    Value *emit_offset(Ast_Array_Reference *ref, Type_Info_Array *array_info,
                       Ast_Block *ast_block);
    void emit_stmts(Ast_Statement *stmt, Ast_Block *ast_block);
    Function_Call *emit_function_call(Ast_Procedure_Call *call_node,
                                      bool32 has_return_value,
                                      Ast_Block *ast_block);
    Block_And_Branch *short_circuit(Ast_Node *root, Basic_Block *left,
                                    Basic_Block *right, bool first_call = true);
    Basic_Block *emit_cond_expr(Ast_Node *cond, Basic_Block *true_target,
                                Basic_Block *false_target, Ast_Block *ast_block,
                                bool is_loop_header = false);

    void save_variable(Ast_Declaration *decl, Value *value,
                       Basic_Block *belongs_to_block);
    Value *read_variable(Ast_Declaration *decl, Basic_Block *block);
    Value *read_variable_recursively(Ast_Declaration *decl, Basic_Block *block);
    Value *add_phi_operands(Ast_Declaration *decl, Phi *phi,
                            Basic_Block *block);

    void seal_block(Basic_Block *block);
};

Ast_Node *curr_ast_node; //

Array<Block_And_Value *> condition_blocks;

Block_And_Branch *IR_Generator::short_circuit(Ast_Node *root, Basic_Block *left,
                                              Basic_Block *right,
                                              bool first_call) {
    if (first_call)
        condition_blocks.release();
    if (root->type == Ast_Type_Binary_Op) {
        Block_And_Branch *returnvalue = new Block_And_Branch;
        auto branch_inst = new Instruction_Branch;
        returnvalue->second = branch_inst;
        auto temp = (Ast_Binary_Op *)root;
        if (temp->op != BINARY_LOGICAL_AND && temp->op != BINARY_LOGICAL_OR) {
            Basic_Block *pred_condition = new Basic_Block;
            push_block(f, pred_condition);
            pred_condition->is_condition_block = true;
            Block_And_Value *s = new Block_And_Value;
            s->first = pred_condition;
            s->second = root;
            condition_blocks.push(s);
            branch_inst->false_target = left;
            branch_inst->true_target = right;
            insert(pred_condition, (Value *)branch_inst);
            returnvalue->first = pred_condition;

            return returnvalue;
        }
        Block_And_Branch *condition_great;
        auto condition_less =
            short_circuit((Ast_Node *)temp->rhs, left, right, false);
        if (temp->op == BINARY_LOGICAL_AND) {
            condition_great = short_circuit((Ast_Node *)temp->lhs, left,
                                            condition_less->first, false);
            returnvalue->second->false_target = left;
            returnvalue->second->true_target = right;
        }
        if (temp->op == BINARY_LOGICAL_OR) {
            condition_great = short_circuit(
                (Ast_Node *)temp->lhs, condition_less->first, right, false);
            returnvalue->second->false_target = left;
            returnvalue->second->true_target = right;
        }

        returnvalue->first = condition_great->first;
        return returnvalue; // MAYBE ERROR
    } else {
        // this mean only one condition???
        Block_And_Branch *returnvalue = new Block_And_Branch;
        Basic_Block *pred_condition = new Basic_Block;
        push_block(f, pred_condition);
        pred_condition->is_condition_block = true;
        Block_And_Value *s = new Block_And_Value;
        s->first = pred_condition;
        s->second = root;
        condition_blocks.push(s);
        auto branch_inst = new Instruction_Branch;
        branch_inst->false_target = left;
        branch_inst->true_target = right;
        insert(pred_condition, (Value *)branch_inst);
        returnvalue->first = pred_condition;
        returnvalue->second = branch_inst;
        return returnvalue;
    }
}

// returns the first condition block
Basic_Block *IR_Generator::emit_cond_expr(Ast_Node *cond,
                                          Basic_Block *true_target,
                                          Basic_Block *false_target,
                                          Ast_Block *ast_block,
                                          bool is_loop_header) {

    auto conditions = short_circuit(cond, false_target, true_target);
    if (is_loop_header) {
        conditions->first->sealed = false;
        unsealed_blocks.push(conditions->first);
    }
    connect_CFG(bb, conditions->first, true);
    int32 length = condition_blocks.size();
    while (length != 0) {
        auto k = condition_blocks[length - 1];
        int32 l = k->first->insts.size();
        while (l != 0) {
            auto temp = k->first->insts[l - 1];
            if (temp->type == INST_BRANCH) {
                connect_CFG(k->first,
                            ((Instruction_Branch *)temp)->true_target);
                connect_CFG(k->first,
                            ((Instruction_Branch *)temp)->false_target);
                break;
            }
            l--;
        }
        length--;
    }
    length = condition_blocks.size();
    while (length != 0) {
        auto k = condition_blocks[length - 1];
        auto old_bb = bb;
        bb = k->first;
        Value *cond_val = emit_value(k->second, ast_block);
        bb = old_bb;
        int32 l = k->first->insts.size();
        Instruction_Branch *g;
        while (l != 0) {
            auto temp = k->first->insts[l - 1];
            if (temp->type == INST_BRANCH) {
                g = (Instruction_Branch *)temp;
                g->cond = new_use(cond_val, (Value *)g);
                g->comment = print_ast_for_ir((Ast_Node *)k->second);
            }
            l--;
        }
        k->first->insts.remove(g);
        k->first->insts.push(g);
        length--;
    }

    return conditions->first;
}

Procedure_IR *IR_Generator::emit_procedure(Ast_Procedure *proc_ast) {
    f = new Procedure_IR;

    // how many arguments does this function have?
    auto func_info = (Type_Info_Procedure *)(proc_ast->proc_type);
    f->args_count = func_info->argument_types.len;

    // name of the function
    f->name = proc_ast->proc_id->name;

    f->start_block = new Basic_Block(f);
    f->exit_block = new Basic_Block; // exit block get pushed in the end

    // place arguments at the start block
    auto decls = proc_ast->proc_body->declarations;
    for (auto decl : decls) {
        if (decl->arg_index >= 0) {
            auto arg_value = new Argument;
            arg_value->decl = decl;
            arg_value->comment = print_ast_for_ir((Ast_Node *)decl);
            arg_value->arg_index = decl->arg_index;

            insert(f->start_block, arg_value);
            f->arguments.push(arg_value);
            save_variable(decl, arg_value, f->start_block);
        }
    }

    // emitting function body
    auto scope = proc_ast->proc_body;
    auto first_stmt = proc_ast->proc_body->first_statement;
    bb = f->start_block;
    emit_stmts(first_stmt, scope);

    // sealing all headers, backwards
    for (int i = unsealed_blocks.len - 1; i >= 0; i--) {
        seal_block(unsealed_blocks[i]);
    }

    push_block(f, f->exit_block);

    remove_trivial_phi(f);
    renumber_blocks(f);
    renumber_values(f);
    // compute_CFG(f);

    return f;
}

Function_Call *IR_Generator::emit_function_call(Ast_Procedure_Call *call_node,
                                                bool32 has_return_value,
                                                Ast_Block *ast_block) {

    auto call_inst = new Function_Call;
    call_inst->comment = print_ast_for_ir((Ast_Node *)call_node);

    // emitting arguemnt values
    for (auto arg_expr : call_node->arguments) {
        Value *arg_val = emit_value(arg_expr, ast_block);
        call_inst->arguments.push(new_use(arg_val, (Value *)call_inst));
    }

    call_inst->name = call_node->procedure_id->name;
    call_inst->has_return_value = has_return_value;
    insert(bb, call_inst);

    return call_inst;
}

// @CLEANUP
void IR_Generator::emit_stmts(Ast_Statement *stmt, Ast_Block *ast_block) {

    for (; stmt; stmt = stmt->next) {

        auto node = stmt->body;

        // handle empty statement
        if (!node)
            continue;

        switch (node->type) {
        case Ast_Type_Identifier:
        case Ast_Type_Integer_Literal: {
        } break;

        // take care of 0+f(), -f() or even A[f()]
        // which can have side effects
        case Ast_Type_Array_Reference:
        case Ast_Type_Binary_Op:
        case Ast_Type_Unary_Op: {
            emit_value(node, ast_block);
        } break;

        case Ast_Type_Block: {
            auto next_ast_block = (Ast_Block *)node;

            auto new_bb = new Basic_Block(f, bb);

            bb = new_bb;
            emit_stmts(next_ast_block->first_statement, next_ast_block);

            if (!bb)
                return;

            auto after_bb = new Basic_Block(f, bb);
            bb = after_bb;

        } break;

        case Ast_Type_Continue: {
            connect_CFG(bb, while_headers.back(), true);
            bb = NULL;
            return;
        } break;

        case Ast_Type_Break: {
            connect_CFG(bb, while_ends.back(), true);
            bb = NULL;
            return;
        } break;

        case Ast_Type_Procedure_Call: {
            auto call_node = (Ast_Procedure_Call *)node;
            emit_function_call(call_node, false, ast_block);
        } break;

        case Ast_Type_Procedure_Return: {
            auto ret_node = (Ast_Procedure_Return *)node;

            auto ret_inst = new Instruction_Return;
            ret_inst->comment = print_ast_for_ir(node);

            if (ret_node->return_value) {
                Value *res = emit_value(ret_node->return_value, ast_block);
                ret_inst->return_value = new_use(res, (Value *)ret_inst);
            }

            insert(bb, ret_inst);
            connect_CFG(bb, f->exit_block);
            bb = NULL;
            return;
        } break;

        case Ast_Type_Declaration_List: {
            auto decl_list = (Ast_Declaration_List *)node;
            for (auto decl_node : decl_list->decls) {

                // @Cleanup
                if (decl_node->decl_type->tag == TYPE_ARRAY) {
                    Value *res = emit_value((Ast_Node *)decl_node, ast_block);
                    res->comment = print_ast_for_ir((Ast_Node *)decl_node);
                    save_variable(decl_node, res, bb);
                } else {
                    Ast_Node *init_value = decl_node->initial_value;
                    if (!init_value) {
                        auto int_node = new Ast_Integer_Literal;
                        int_node->value = 0;
                        init_value = (Ast_Node *)int_node;
                    }
                    Value *res = emit_value(init_value, ast_block);
                    res->comment = print_ast_for_ir((Ast_Node *)decl_node);
                    save_variable(decl_node, res, bb);
                }
            }
        } break;

        case Ast_Type_Assignment: {
            auto assign_node = (Ast_Assignment *)node;

            Value *value_to_assign =
                emit_value(assign_node->value_to_assign, ast_block);
            if (assign_node->lhs->type == Ast_Type_Array_Reference) {
                auto ref = (Ast_Array_Reference *)assign_node->lhs;
                auto decl_node = find_declaration(ast_block, ref->array_id,
                                                  &program_ast->globals);

                Value *base = read_variable(decl_node, bb);
                Type_Info_Array *arr_info =
                    (Type_Info_Array *)decl_node->decl_type;
                assert(arr_info->dimension.len == ref->indices.len);
                if (arr_info->dimension.len != ref->indices.len) {
                    exit(24);
                }
                Value *offset = emit_offset(ref, arr_info, ast_block);

                auto mem_write = new Memory_Write;
                mem_write->comment = print_ast_for_ir((Ast_Node *)assign_node);
                mem_write->decl = decl_node;
                mem_write->base = new_use(base, (Value *)mem_write);
                mem_write->offset = new_use(offset, (Value *)mem_write);
                mem_write->value_to_write =
                    new_use(value_to_assign, (Value *)mem_write);

                insert(bb, mem_write);

            } else {
                value_to_assign->comment =
                    print_ast_for_ir((Ast_Node *)assign_node);
                auto lhs_id = (Ast_Identifier *)assign_node->lhs;
                auto decl_node =
                    find_declaration(ast_block, lhs_id, &program_ast->globals);

                if (decl_node->is_global) {
                    auto global_address = read_variable(decl_node, bb);
                    auto mem_write = new Memory_Write;
                    mem_write->decl = decl_node;
                    mem_write->comment =
                        print_ast_for_ir((Ast_Node *)decl_node->id);
                    mem_write->base =
                        new_use(global_address, (Value *)mem_write);
                    mem_write->value_to_write =
                        new_use(value_to_assign, (Value *)mem_write);

                    insert(bb, mem_write);
                } else {
                    save_variable(decl_node, value_to_assign, bb);
                }
            }

        } break;

        case Ast_Type_While: {

            auto while_node = (Ast_While *)node;

            auto landing_pad = new Basic_Block(f, bb);
            auto header = new Basic_Block(f, landing_pad);
            auto body = new Basic_Block(f);
            auto while_end = new Basic_Block;

            // creat some empty block for loop unrolling one occur after emit
            // cond expr ,another is in tail prepared for creating another while
            Basic_Block *empty1 = new Basic_Block(f);
            Basic_Block *empty2 = new Basic_Block(f);

            bb = header;
            header = emit_cond_expr(while_node->condition, empty1, empty2,
                                    ast_block, true);

            header->is_loop_header = true;

            connect_CFG(empty1, body, true);
            connect_CFG(empty2, while_end, true);

            // emit body
            Basic_Block *represent_header = new Basic_Block;
            while_headers.push(represent_header);
            while_ends.push(while_end);

            auto body_block = while_node->while_block;
            bb = body;
            emit_stmts(body_block->first_statement, body_block);

            while_headers.pop();
            while_ends.pop();
            if (bb) {
                connect_CFG(bb, represent_header, true);
            }
            if (represent_header->preds.len != 0) {
                push_block(f, represent_header);
                connect_CFG(represent_header, header, true);
            }

            // emit while end
            push_block(f, while_end);
            bb = while_end;
            emit_stmts(stmt->next, ast_block);

            return;
        } break;

        case Ast_Type_If: {

            auto ast_if = (Ast_If *)node;

            auto if_bb = new Basic_Block, then_bb = new Basic_Block;

            emit_cond_expr(ast_if->condition, if_bb, then_bb, ast_block);

            push_block(f, if_bb);
            push_block(f, then_bb);

            // emit if block
            bb = if_bb;
            emit_stmts(ast_if->if_block->first_statement, ast_if->if_block);

            Basic_Block *if_bb_exit = bb, *then_bb_exit = then_bb;

            // emit then block
            if (ast_if->else_block) {
                bb = then_bb;
                emit_stmts(ast_if->else_block->first_statement,
                           ast_if->else_block);
                then_bb_exit = bb;
            }

            // if at least one of true block and false block
            // is unfinished, meaning the control flow can reach if_end,
            // go on emiiting the code after the if stmt
            if (if_bb_exit || then_bb_exit) {
                // can reach to the statements after if stmt
                auto if_end = new Basic_Block(f);

                if (if_bb_exit)
                    connect_CFG(if_bb_exit, if_end, true);
                if (then_bb_exit)
                    connect_CFG(then_bb_exit, if_end, true);

                bb = if_end;
                emit_stmts(stmt->next, ast_block);
            } else {
                bb = NULL;
            }

            return;

        } break;

        default: {
            exit(30);
            report_error("Internal Compiler Error: translating unsupported "
                         "language construct to IR.");
        }
        }
    }
}

Value *IR_Generator::emit_value(Ast_Node *node, Ast_Block *ast_block) {
    if (node->type == Ast_Type_Binary_Op) {
        auto binary_node = (Ast_Binary_Op *)node;

        auto binary_inst = new Instruction_Binary;

        binary_inst->op_type = binary_node->op;

        binary_inst->lhs = new_use(emit_value(binary_node->lhs, ast_block),
                                   (Value *)binary_inst);
        binary_inst->rhs = new_use(emit_value(binary_node->rhs, ast_block),
                                   (Value *)binary_inst);
        insert(bb, (Value *)binary_inst);

        return (Value *)binary_inst;
    } else if (node->type == Ast_Type_Unary_Op) {
        auto unary_inst = new Instruction_Unary;
        auto unary_node = (Ast_Unary_Op *)node;

        // @Optimization: ignore unary positive
        if (unary_node->op == UNARY_POSITIVE) {
            return emit_value(unary_node->operand, ast_block);
        }

        auto opr_node = unary_node->operand;

        unary_inst->op_type = unary_node->op;
        auto unary_val = emit_value(unary_node->operand, ast_block);
        unary_inst->oprend = new_use(unary_val, (Value *)unary_inst);
        insert(bb, (Value *)unary_inst);
        return (Value *)unary_inst;
    } else if (node->type == Ast_Type_Integer_Literal) {
        auto unary_inst = new Instruction_Unary;
        auto int_node = (Ast_Integer_Literal *)node;
        auto const_val = new Constant(int_node->value);
        insert(bb, (Value *)const_val);
        return (Value *)const_val;
    } else if (node->type == Ast_Type_Identifier) {
        auto id_node = (Ast_Identifier *)node;
        auto decl = find_declaration(ast_block, id_node, &program_ast->globals);

        auto value = read_variable(decl, bb);

        if (decl->is_const && decl->decl_type->tag != TYPE_ARRAY) {
            auto constant =
                evaluate(ast_block, decl->initial_value, program_ast->globals);
            auto const_val = new Constant(constant);
            insert(bb, const_val);
            return const_val;
        }

        if (decl->is_global) {
            if (decl->decl_type->tag == TYPE_ARRAY) {
                return value;
            } else {
                auto mem_read = new Memory_Read;
                mem_read->decl = decl;
                mem_read->base = new_use(value, (Value *)mem_read);
                mem_read->comment = print_ast_for_ir(node);
                mem_read->source = node;
                insert(bb, (Value *)mem_read);
                return (Value *)mem_read;
            }
        } else {
            return value;
        }
    } else if (node->type == Ast_Type_Declaration) {
        auto decl = (Ast_Declaration *)node;
        // assert(decl->decl_type->tag == TYPE_ARRAY);
        if (decl->decl_type->tag != TYPE_ARRAY) {
            exit(25);
        }
        auto ref_info = (Type_Info_Array *)decl->decl_type;

        int array_size = 1;
        for (auto d : ref_info->dimension) {
            array_size *= d;
        }

        // 4 bytes for each int
        array_size *= 4;

        auto size_val = new Constant(array_size);
        insert(bb, (Value *)size_val);
        auto alloca = new Alloca;
        alloca->decl = decl;
        alloca->size = new_use((Value *)size_val, (Value *)alloca);
        insert(bb, (Value *)alloca);

        // perform initialization sequence
        if (decl->initial_value) {
            auto init_list = (Ast_Init_List *)decl->initial_value;
            auto init_seq = get_init_sequence(ref_info->dimension, init_list);

            auto current_index = 0;
            for (auto init_inst : init_seq) {

                Value *cause = NULL;

                if (init_inst.tag == ZEROS) {
                    // emit a memset function call to clear the memory
                    auto offset = new Constant(current_index * 4);
                    insert(bb, (Value *)offset);

                    auto base = new Instruction_Binary;
                    base->op_type = BINARY_ADD;
                    base->lhs = new_use(alloca, base);
                    base->rhs = new_use(offset, base);
                    insert(bb, (Value *)base);

                    auto memset_call = new Function_Call;
                    memset_call->name = (char *)"memset";
                    memset_call->has_return_value = false;
                    cause = memset_call;

                    auto byte_to_set = new Constant(0);
                    insert(bb, (Value *)byte_to_set);

                    auto num_of_bytes =
                        new Constant(init_inst.num_of_zeros * 4);
                    insert(bb, (Value *)num_of_bytes);

                    memset_call->arguments.push(new_use(base, memset_call));
                    memset_call->arguments.push(
                        new_use(byte_to_set, memset_call));
                    memset_call->arguments.push(
                        new_use(num_of_bytes, memset_call));

                    insert(bb, (Value *)memset_call);

                    current_index += init_inst.num_of_zeros;

                } else {
                    // emit a Memory_Write to M[base+current_index]
                    auto offset = new Constant(current_index);
                    auto mem_write = new Memory_Write;
                    insert(bb, (Value *)offset);
                    cause = mem_write;

                    mem_write->base = new_use(alloca, mem_write);
                    mem_write->decl = decl;
                    mem_write->offset = new_use(offset, mem_write);
                    mem_write->value_to_write = new_use(
                        emit_value(init_inst.init_value, ast_block), mem_write);
                    insert(bb, (Value *)mem_write);

                    current_index++;
                }
            }
        }

        return (Value *)alloca;
    } else if (node->type == Ast_Type_Array_Reference) {
        auto ref = (Ast_Array_Reference *)node;

        auto array_decl =
            find_declaration(ast_block, ref->array_id, &program_ast->globals);
        auto ref_info = (Type_Info_Array *)array_decl->decl_type;
        Type_Info_Array *arr_info = (Type_Info_Array *)array_decl->decl_type;

        Value *base = read_variable(array_decl, bb);
        Value *offset = emit_offset(ref, arr_info, ast_block);

        if (arr_info->dimension.len == ref->indices.len) {

            auto mem_read = new Memory_Read;
            mem_read->decl = array_decl;
            mem_read->base = new_use(base, mem_read);
            mem_read->offset = new_use(offset, mem_read);
            mem_read->comment = print_ast_for_ir(node);
            mem_read->source = (Ast_Node *)ref;
            insert(bb, (Value *)mem_read);

            return (Value *)mem_read;
        } else {

            auto const_2 = new Constant(2);

            auto mul4 = new Instruction_Binary;
            mul4->op_type = BINARY_LSL;
            mul4->lhs = new_use(offset, (Value *)mul4);
            mul4->rhs = new_use(const_2, (Value *)mul4);

            auto add = new Instruction_Binary;
            add->op_type = BINARY_ADD;
            add->lhs = new_use(base, (Value *)add);
            add->rhs = new_use(mul4, (Value *)add);

            insert(bb, const_2);
            insert(bb, mul4);
            insert(bb, add);

            return add;
        }

    } else if (node->type == Ast_Type_Procedure_Call) {
        auto call_node = (Ast_Procedure_Call *)node;
        auto call_inst = emit_function_call(call_node, true, ast_block);
        return (Value *)call_inst;
    }

    exit(17);
    assert(false);
    return NULL;
}

// emit calcuations of offset
// e.g. for int A[20][10][5];
// and we need to know the offset of A[10][5][2]
// the result of this function is a value of 10*(10*5) + 5*(5) + 2
Value *IR_Generator::emit_offset(Ast_Array_Reference *ref,
                                 Type_Info_Array *array_info,
                                 Ast_Block *ast_block) {

    auto ref_dims = ref->indices;
    auto decl_dims = array_info->dimension;

    int REF_DIMS = ref->indices.len;
    int DECL_DIMS = array_info->dimension.len;

    int pitch = 1;
    for (int32 i = 0; i < DECL_DIMS; i++)
        pitch *= decl_dims[i];

    Value *offset = NULL;

    for (int32 i = 0; i < REF_DIMS; i++) {

        pitch /= decl_dims[i];

        Value *index = emit_value(ref_dims[i], ast_block);

        if (i == 0) {
            // emit ref_dims[0] * pitch
            if (pitch != 1) {
                auto pitch_value = new Constant(pitch);
                insert(bb, pitch_value);
                auto first_mul = new Instruction_Binary;
                first_mul->op_type = BINARY_MULTIPLY;
                first_mul->lhs = new_use(index, first_mul);
                first_mul->rhs = new_use(pitch_value, first_mul);

                offset = (Value *)first_mul;
                insert(bb, first_mul);
            } else {
                offset = index;
            }

        } else {

            Value *value_to_add = index;

            if (pitch != 1) {
                auto pitch_value = new Constant(pitch);
                insert(bb, pitch_value);
                auto next_mul = new Instruction_Binary;
                next_mul->op_type = BINARY_MULTIPLY;
                next_mul->lhs = new_use(index, next_mul);
                next_mul->rhs = new_use(pitch_value, next_mul);
                insert(bb, next_mul);

                value_to_add = next_mul;
            }

            auto add = new Instruction_Binary;
            add->op_type = BINARY_ADD;
            add->lhs = new_use(offset, add);
            add->rhs = new_use(value_to_add, add);
            insert(bb, add);

            offset = (Value *)add;
        }
    }

    return offset;
}

uint32 IR_Generator::insert(Basic_Block *basic_block, Value *v,
                            bool insert_to_first) {
    v->n = curr_value_index;
    v->b = basic_block;
    if (insert_to_first) {
        basic_block->insts.insert(0, v);
    } else {
        basic_block->insts.push(v);
    }
    return curr_value_index++;
}

void IR_Generator::save_variable(Ast_Declaration *decl, Value *value,
                                 Basic_Block *block) {
    auto it = decl->value_map.find(block);
    if (it != decl->value_map.end()) {
        it->second = value;
    } else {
        decl->value_map[block] = value;
    }
}

Value *IR_Generator::read_variable(Ast_Declaration *decl, Basic_Block *block) {
    if (decl->is_global) {
        // @note: emits a global ref at use site
        // and hope GVN will hoist it
        auto global = new Global();
        global->decl = decl;
        global->name = decl->id->name; // @Ouch
        insert(block, global);

        return global;
    }

    auto it = decl->value_map.find(block);

    if (it != decl->value_map.end()) {
        return it->second;
    } else {
        return read_variable_recursively(decl, block);
    }
}

Value *IR_Generator::read_variable_recursively(Ast_Declaration *decl,
                                               Basic_Block *block) {
    if (block == f->start_block) {
        exit(13);
        assert(false && "use before definition");
        return new Constant(44);
    }

    Value *val;
    if (!block->sealed) {
        auto phi = new Phi(block);
        phi->decl = decl;
        phi->comment = print_ast_for_ir((Ast_Node *)decl->id);
        auto p = Pair<Ast_Declaration *, Phi *>(decl, phi);
        block->incomplete_phis.push(p);
        val = (Value *)phi;
    } else if (block->preds.len == 1) {
        val = read_variable(decl, block->preds[0]);
    } else {
        auto phi = new Phi(block);
        phi->decl = decl;
        phi->comment = print_ast_for_ir((Ast_Node *)decl->id);
        save_variable(decl, phi, block);
        val = add_phi_operands(decl, phi, block);
    }
    save_variable(decl, val, block);

    return val;
}

void IR_Generator::seal_block(Basic_Block *block) {
    block->sealed = true;
    for (auto p : block->incomplete_phis) {
        add_phi_operands(p.first, (Phi *)p.second, block);
    }
    block->incomplete_phis.release();
}

Value *IR_Generator::add_phi_operands(Ast_Declaration *decl, Phi *phi,
                                      Basic_Block *block) {

    for (int i = 0; i < block->preds.len; i++) {
        Value *operand = NULL;
        operand = read_variable(decl, block->preds[i]);
        phi->operands.push(new_use(operand, phi));
        phi->sources.push(block->preds[i]);
    }

    insert(block, phi, true);
    return phi;
}

void remove_trivial_phi(Procedure_IR *f) {

    Array<Phi *> phi_worklist;
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            if (auto phi = v->as<Phi>()) {
                phi_worklist.push(phi);
            }
            if (auto phi = v->as<Memory_Phi>()) {
                phi_worklist.push((Phi *)phi);
            }
        }
    }

    while (!phi_worklist.empty()) {
        auto phi = phi_worklist.back();
        phi_worklist.pop();

        // already optimized out
        if (phi->b == NULL)
            continue;

        Value *same = NULL;
        for (auto op_use : phi->operands) {
            Value *op = op_use->v;
            if (op == same || (op == (Value *)phi))
                continue;
            if (same != NULL)
                goto dont_optimize_phi;
            same = op;
        }

        for (auto u = phi->use; u; u = u->next) {
            if (u->user != (Value *)phi) {
                if (auto user_phi = u->user->as<Phi>()) {
                    phi_worklist.push(user_phi);
                }
                if (auto user_phi = u->user->as<Memory_Phi>()) {
                    phi_worklist.push((Phi *)user_phi);
                }
            }
        }

        phi->replace_all_uses_with(same);

        // update trash
        if (phi->type == MEMORY_PHI) {
            for (auto it = f->trash.begin(); it != f->trash.end(); it++) {

                auto v = it->first;
                if (v == phi) {
                    for (auto it2 = it->second.begin(); it2 != it->second.end();
                         it2++) {
                        auto b = it2->first;
                        for (auto it3 = it2->second.begin();
                             it3 != it2->second.end(); it3++) {
                            auto decl = it3->first;
                            if (f->trash[phi][b][decl]) {
                                f->trash[same][b][decl] = true;
                            }
                        }
                    }
                }
            }
        }

        for (auto opr : phi->operands)
            opr->remove();

        phi->b->insts.remove(phi);
        phi->b = NULL;

    dont_optimize_phi:;
    }
}

Value *try_remove_trivial_phi(Phi *phi, Ast_Declaration *decl) {
    Value *same = NULL;
    for (auto op_use : phi->operands) {
        Value *op = op_use->v;
        if (op == same || (op == (Value *)phi))
            continue;
        if (same != NULL)
            return (Value *)phi;
        same = op;
    }

    // @note: same == null?

    // found trivial phi

    Array<Value *> users;
    for (auto u = phi->use; u; u = u->next) {
        if (u->user != (Value *)phi) {
            users.push(u->user);
        }
    }
    phi->replace_all_uses_with(same);

    // remove trivial phi's uses of its operand
    for (auto opr : phi->operands) {
        opr->remove();
    }

    for (auto user : users) {
        if (auto next_phi = user->as<Phi>()) {
            auto optimized_value = try_remove_trivial_phi(
                next_phi, next_phi->decl); // @TODO: swap problem?

            if (optimized_value != user) {
                // replace all defs in blocks
                for (auto it = next_phi->decl->value_map.begin();
                     it != next_phi->decl->value_map.end(); it++) {

                    if (it->second == user) {
                        it->second = optimized_value;
                    }
                }
                user->b->insts.remove(user);
                // user->b = NULL;
                // delete user;
            }
        }
    }

    return same;
}

/* Utility Functions */

void insert_br(Basic_Block *from, Basic_Block *to) {
    auto direct_br = new Instruction_Direct_Branch;
    direct_br->target = to;
    direct_br->b = from;
    /*assert(from->insts.len == 0 || (from->insts.back()->type !=
       INST_DIRECT_BRANCH && from->insts.back()->type != INST_BRANCH));*/
    from->insts.push((Value *)direct_br);
}

void connect_CFG(Basic_Block *from, Basic_Block *to, bool insert_br_to_from) {
    if (from->succs.find(to) != -1) {
        // assert(false && "adding existing edge to CFG");
        exit(14);
        printf("adding existing edge to CFG");
        return;
    }
    if (from == to) {
        exit(15);
        printf("connecting to itself");
        // assert(false && "connecting to itself");
    }
    to->preds.push(from);
    from->succs.push(to);
    if (insert_br_to_from) {
        insert_br(from, to);
    }
}

Use *new_use(Value *value, Value *user) {
    auto use = new Use;
    use->v = value;
    use->user = user;
    value->add_new_use(use);
    return use;
}

// @FIXME: clean this up
void destroy_use(Use *use) {
    use->remove();
    delete use;
}

Branch_Condition binary_op_to_branch_cond(Binary_Op_Type op_type) {
    switch (op_type) {
    case BINARY_LESS_THAN: {
        return LESS_THAN;
    } break;
    case BINARY_GREATER_THAN: {
        return GREATER_THAN;
    } break;
    case BINARY_LESS_THAN_OR_EQUAL_TO: {
        return LESS_THAN_OR_EQUAL;
    } break;
    case BINARY_GREATER_THAN_OR_EQUAL_TO: {
        return GREATER_THAN_OR_EQUAL;
    } break;
    case BINARY_NOT_EQUAL_TO: {
        return NOT_EQUAL;
    } break;
    case BINARY_EQUAL_TO: {
        return EQUAL;
    } break;
    default: {
        exit(31);
        report_error("Internal Compiler Error: converting unknown binary op "
                     "type to branch type.");
    }
    }
    return NO_CONDITION;
}

// @Hack
Branch_Condition invert_branch_cond(Branch_Condition cond) {
    if (cond == NO_CONDITION)
        return cond;
    if (cond >= 1 && cond <= 3) {
        return (Branch_Condition)(cond + 3);
    } else if (cond >= 4 && cond <= 6) {
        return (Branch_Condition)(cond - 3);
    }
    assert(false);
    return NO_CONDITION;
}

const char *get_branch_suffix(Branch_Condition condition) {
    switch (condition) {
    case NO_CONDITION: {
        return "";
    } break;
    case LESS_THAN: {
        return "lt";
    } break;
    case GREATER_THAN: {
        return "gt";
    } break;
    case LESS_THAN_OR_EQUAL: {
        return "le";
    } break;
    case GREATER_THAN_OR_EQUAL: {
        return "ge";
    } break;
    case NOT_EQUAL: {
        return "ne";
    } break;
    case EQUAL: {
        return "eq";
    } break;
    default: {
        assert(false);
        return "";
    }
    }
}

// this is for debugging
// don't care about performance
// but what happens when returning string from a function? @Performance
std::string print_basic_block(Basic_Block *bb, bool is_cfg_viewer) {
    std::stringstream p;
    const char *newline = is_cfg_viewer ? "\\l" : "\n";
    const char *le = is_cfg_viewer ? "\\<=" : "<=";
    const char *ge = is_cfg_viewer ? "\\>=" : ">=";
    const char *lt = is_cfg_viewer ? "\\<" : "<";
    const char *gt = is_cfg_viewer ? "\\>" : ">";
    const char *lsl = is_cfg_viewer ? "\\<\\<" : "<<";
    const char *rsl = is_cfg_viewer ? "\\>\\>" : ">>";
    const char *logical_or = is_cfg_viewer ? "\\|\\|" : "||";
    const char *phi_sym = is_cfg_viewer ? "𝜙" : "phi";
    for (int i = 0; i < bb->insts.len; i++) {
        if (!is_cfg_viewer)
            p << "    ";
        Value *v = bb->insts[i];

        switch (v->type) {
        case CONSTANT: {
            auto const_val = (Constant *)v;

            p << 'v' << v->n << " := " << const_val->value;
        } break;

        case ARGUMENT: {
            auto arg = (Argument *)v;

            p << 'v' << v->n << " := arg " << arg->arg_index;
        } break;

        case GLOBAL: {
            auto global = (Global *)v;

            p << 'v' << v->n << " := global: " << global->name;
        } break;

        case INST_BINARY: {
            auto bi_inst = (Instruction_Binary *)v;

            p << 'v' << v->n << " := " << 'v' << bi_inst->lhs->v->n << ' ';
            switch (bi_inst->op_type) {
            case BINARY_ADD: {
                p << '+';
            } break;
            case BINARY_SUBTRACT: {
                p << '-';
            } break;
            case BINARY_MULTIPLY: {
                p << '*';
            } break;
            case BINARY_DIVIDE: {
                p << '/';
            } break;
            case BINARY_MOD: {
                p << "%";
            } break;
            case BINARY_LESS_THAN: {
                p << lt;
            } break;
            case BINARY_GREATER_THAN: {
                p << gt;
            } break;
            case BINARY_LESS_THAN_OR_EQUAL_TO: {
                p << le;
            } break;
            case BINARY_GREATER_THAN_OR_EQUAL_TO: {
                p << ge;
            } break;
            case BINARY_NOT_EQUAL_TO: {
                p << "!=";
            } break;
            case BINARY_EQUAL_TO: {
                p << "==";
            } break;
            case BINARY_LOGICAL_OR: {
                p << logical_or;
            } break;
            case BINARY_LOGICAL_AND: {
                p << "&&";
            } break;
            case BINARY_LSL: {
                p << lsl;
            } break;
            case BINARY_LSR: {
                p << rsl;
            } break;
            default: {
                exit(19);
                assert(false && "converting unsupported binary operator to IR");
            }
            }
            p << " v" << bi_inst->rhs->v->n;

        } break;
        case INST_UNARY: {
            auto bi_inst = (Instruction_Unary *)v;

            p << 'v' << v->n << " := ";
            switch (bi_inst->op_type) {
            case UNARY_POSITIVE: {
                p << ' ';
            } break;
            case UNARY_NEGATIVE: {
                p << '-';
            } break;
            case UNARY_NOT: {
                p << '!';
            } break;
            default: {
                exit(20);
                assert(false && "converting unsupported unary operator to IR");
            }
            }
            p << "v" << bi_inst->oprend->v->n;

        } break;
        case INST_RETURN: {
            auto ret_inst = (Instruction_Return *)v;
            p << "ret";
            if (ret_inst->return_value) {
                p << " v" << ret_inst->return_value->v->n;
            }
        } break;

        case INST_BRANCH: {
            auto br_inst = (Instruction_Branch *)v;
            p << "br v" << br_inst->cond->v->n << " ? B"
              << br_inst->true_target->index_in_procedure << " : B"
              << br_inst->false_target->index_in_procedure;
        } break;

        case INST_DIRECT_BRANCH: {
            auto br_inst = (Instruction_Direct_Branch *)v;
            p << "br B" << br_inst->target->index_in_procedure;
        } break;

        case MEMORY_PHI:
        case PHI: {
            auto phi = (Phi *)v;
            p << 'v' << phi->n << " := " << phi_sym << "(";
            for (int i = 0; i < phi->operands.len; i++) {
                p << "[";
                p << 'v' << phi->operands[i]->v->n;
                p << ", ";
                p << "B" << phi->sources[i]->index_in_procedure;
                p << "]";
                if (i != phi->operands.len - 1)
                    p << ", ";
            }

            p << ")";
        } break;

        case ALLOCA: {
            auto alloca = (Alloca *)v;
            int array_size = ((Constant *)(alloca->size->v))->value;
            p << "v" << v->n << " := alloca(" << array_size << ")";
        } break;

        case MEMORY_READ: {
            auto mem_read = (Memory_Read *)v;
            p << "v" << v->n << " := M[v" << mem_read->base->v->n;
            if (mem_read->offset) {
                p << " + v" << mem_read->offset->v->n;
            }
            if (mem_read->mem_ver) {
                p << ", "
                  << "v" << mem_read->mem_ver->v->n;
            }
            p << "]";
        } break;

        case MEMORY_WRITE: {
            auto mem_write = (Memory_Write *)v;
            p << "M[v" << mem_write->base->v->n;
            if (mem_write->offset) {
                p << " + v" << mem_write->offset->v->n;
            }
            p << "] = v" << mem_write->value_to_write->v->n;
        } break;

        case MEMORY_DEF: {
            auto def = (Memory_Def *)v;
            p << "v" << v->n;
            if (def->is_initial_version) {
                p << " := init mem";
            } else {
                assert(def->clobbers);
                p << " := "
                  << "v" << def->cause->v->n << " clobbers v"
                  << def->clobbers->v->n;
            }

        } break;

        case FUNCTION_CALL: {
            auto call = (Function_Call *)v;

            if (call->has_return_value) {
                p << "v" << call->n << " := ";
            }

            p << "@" << call->name << "(";
            for (int i = 0; i < call->arguments.len; i++) {
                p << "v" << call->arguments[i]->v->n;
                if (i != call->arguments.len - 1)
                    p << ", ";
            }

            p << ")";
        } break;

        default: {
            exit(32);
            report_error(
                "Internal Compiler Error: printing unknown IR instrution");
        }
        }

        // TODO: alignment seems like a pain...
        // \t just doesn't work
        // setw and printf formatting doesn't help
        // so I guess we should manually record how long the current line is
        // and add padding afterwards
        if (!is_cfg_viewer && v->comment) {
            if (v->type == CONSTANT || v->type == INST_DIRECT_BRANCH ||
                v->type == INST_RETURN || v->type == ARGUMENT ||
                v->type == INST_UNARY ||
                (v->type == MEMORY_READ && !((Memory_Read *)v)->offset) ||
                (v->type == MEMORY_WRITE && !((Memory_Write *)v)->offset)) {
                p << "\t";
            }
            p << "\t" << v->comment;
        }

        p << newline;
    }
    return p.str();
}

std::string print_procedure_IR(Procedure_IR *proc) {
    std::stringstream p;
    for (auto b : proc->blocks) {
        p << "B" << b->index_in_procedure;
        if (b->preds.len > 0) {
            p << " <- ( ";
            for (auto parent : b->preds) {
                p << "B" << parent->index_in_procedure << ' ';
            }
            p << ")";
        }
        p << ":\n";
        p << print_basic_block(b);
        p << "\n";
    }
    return p.str();
}

// @Cleanup
std::string print_comments(Basic_Block *bb) {
    std::stringstream p;
    for (auto v : bb->insts) {
        if (v->comment) {
            p << v->comment;
        }
        p << "\\l";
    }
    return p.str();
}

void print_use_list(Procedure_IR *f) {
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            printf("v%d is used by:", v->n);
            for (auto u = v->use; u; u = u->next) {
                printf(" v%d", u->user->n);
            }
            printf("\n");
        }
    }
    printf("\n");
}

Array<Use *> get_operands(Value *v) {
    Array<Use *> ops;
    switch (v->type) {

    case INST_BINARY: {
        auto bi = (Instruction_Binary *)v;
        ops.push(bi->lhs);
        ops.push(bi->rhs);
    } break;
    case INST_UNARY: {
        auto bi = (Instruction_Unary *)v;
        ops.push(bi->oprend);
    } break;
    case INST_BRANCH: {
        auto br = (Instruction_Branch *)v;
        ops.push(br->cond);
    } break;

    case INST_RETURN: {
        auto ret = (Instruction_Return *)v;
        if (ret->return_value)
            ops.push(ret->return_value);
    } break;

    case MEMORY_PHI:
    case PHI: {
        auto phi = (Phi *)v;
        for (auto operand : phi->operands) {
            ops.push(operand);
        }
    } break;

    case ALLOCA: {
        auto alloca = (Alloca *)v;
        ops.push(alloca->size);
    } break;

    case MEMORY_DEF: {
        auto mdef = (Memory_Def *)v;
        if (mdef->clobbers)
            ops.push(mdef->clobbers);
        if (mdef->cause)
            ops.push(mdef->cause);
    } break;

    case MEMORY_READ: {
        auto mread = (Memory_Read *)v;
        if (mread->base)
            ops.push(mread->base);
        if (mread->offset)
            ops.push(mread->offset);
        if (mread->mem_ver)
            ops.push(mread->mem_ver);
    } break;

    case MEMORY_WRITE: {
        auto mwrite = (Memory_Write *)v;
        if (mwrite->base)
            ops.push(mwrite->base);
        if (mwrite->offset)
            ops.push(mwrite->offset);
        if (mwrite->value_to_write)
            ops.push(mwrite->value_to_write);
    } break;

    case FUNCTION_CALL: {
        auto call = (Function_Call *)v;
        for (auto arg : call->arguments) {
            ops.push(arg);
        }
    } break;

    case CONSTANT:
    case ARGUMENT:
    case GLOBAL:
    case INST_DIRECT_BRANCH:
        break;

    default:
        exit(21);
        assert(false);
    }
    return ops;
}

bool32 has_return(Basic_Block *bb) {
    //@que bao kong han shu bu hui cuo wu..dog dog dog
    if (bb == NULL)
        return false;
    if (bb->insts.len != 0) {
        auto last_inst = bb->insts[bb->insts.len - 1];
        if (last_inst->type == INST_RETURN) {
            return true;
        }
    }
    return false;
}

bool32 has_branch(Basic_Block *bb) {
    //@que bao kong han shu bu hui cuo wu..dog dog dog
    if (bb == NULL)
        return true;
    if (bb->insts.len != 0) {
        auto last_inst = bb->insts[bb->insts.len - 1];
        if (last_inst->type == INST_RETURN || last_inst->type == INST_BRANCH ||
            last_inst->type == INST_DIRECT_BRANCH) {
            return true;
        }
    }
    return false;
}

Procedure_IR *emit_func_IR(Program_AST *program_ast, Ast_Procedure *proc_ast) {
    IR_Generator gen(program_ast);

    Procedure_IR *IR = gen.emit_procedure(proc_ast);
    auto func_type = (Type_Info_Procedure *)proc_ast->proc_type;
    IR->has_return_value = (func_type->return_type->tag == TYPE_INTEGER);

    return IR;
}

Procedure_IR *new_builtin(const char *name) {
    auto f = new Procedure_IR;
    f->name = name;
    f->has_side_effect = f->is_builtin = true;
    return f;
}

Program_IR *emit_IR(Program_AST *program_ast) {
    auto program_IR = new Program_IR;
    program_IR->globals = program_ast->globals;

    for (auto func_ast : program_ast->procedures) {
        auto func_IR = emit_func_IR(program_ast, func_ast);
        program_IR->procedures.push(func_IR);
    }

    program_IR->procedures.push(new_builtin("_sysy_starttime"));
    program_IR->procedures.push(new_builtin("_sysy_stoptime"));
    program_IR->procedures.push(new_builtin("putint"));
    program_IR->procedures.push(new_builtin("getint"));
    program_IR->procedures.push(new_builtin("getch"));
    program_IR->procedures.push(new_builtin("putch"));
    program_IR->procedures.push(new_builtin("memset"));
    program_IR->procedures.push(new_builtin("getarray"));
    program_IR->procedures.push(new_builtin("putarray"));

    return program_IR;
}

void renumber_blocks(Procedure_IR *f) {
    // move exit block to last, not strictly needed though.
    int e = f->blocks.find(f->exit_block);
    f->blocks[e] = f->blocks[f->blocks.len - 1];
    f->blocks[f->blocks.len - 1] = f->exit_block;

    for (int i = 0; i < f->blocks.len; i++) {
        f->blocks[i]->index_in_procedure = i;
    }
}

void renumber_values(Procedure_IR *f) {
    int values_count = 0;
    for (auto bb : f->blocks) {
        for (auto v : bb->insts) {
            v->n = values_count++;
        }
    }
    f->values_count = values_count;
}

void put_phis_to_start(Basic_Block *bb) {
    bool past_first_non_phi = false;

    for (auto v : bb->insts) {
        if (v->type != PHI && v->type != MEMORY_PHI) {
            past_first_non_phi = true;
            continue;
        }

        if (past_first_non_phi && (v->type == PHI || v->type == MEMORY_PHI)) {
            // @note: it's safe to do this while iterating
            bb->insts.remove(v);
            bb->insts.insert(0, v);
        }
    }
}

int32 find_func_index(Program_IR *prog, const char *func_name) {
    for (int32 i = 0; i < prog->procedures.len; i++) {
        if (strcmp(func_name, prog->procedures[i]->name) == 0) {
            return i;
        }
    }
    return -1; // is builtin function
}

bool is_relational_binary_op(Binary_Op_Type op) {
    return (op == BINARY_LESS_THAN || op == BINARY_LESS_THAN_OR_EQUAL_TO ||
            op == BINARY_GREATER_THAN ||
            op == BINARY_GREATER_THAN_OR_EQUAL_TO || op == BINARY_EQUAL_TO ||
            op == BINARY_NOT_EQUAL_TO);
}
