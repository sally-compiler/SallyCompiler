
Constant *new_const(Procedure_IR *f, int32 c) {
    auto cc = new Constant(c);
    f->start_block->insts.insert(f->start_block->insts.len - 2, cc);
    cc->b = f->start_block;
    return cc;
}

// try to turn (x op y) op z
// intro x op (y op z) if y op z is constant
bool combine_assoc(Procedure_IR *f, Value *v) {
    auto z = v->as<Instruction_Binary>();
    if (!z)
        return false;
    auto zc = z->rhs->v->as<Constant>();
    if (!zc)
        return false;

    auto xy = z->lhs->v->as<Instruction_Binary>();
    if (!xy)
        return false;
    auto xyc = xy->rhs->v->as<Constant>();
    if (!xyc)
        return false;

    if (z->op_type == xy->op_type &&
        (z->op_type == BINARY_ADD || z->op_type == BINARY_SUBTRACT ||
         z->op_type == BINARY_MULTIPLY)) {
        int32 cc;
        switch (z->op_type) {
        case BINARY_ADD:
            cc = zc->value + xyc->value;
            break;
        case BINARY_SUBTRACT:
            cc = zc->value + xyc->value;
            break;
        case BINARY_MULTIPLY:
            cc = zc->value * xyc->value;
            break;
        }
        auto c = new_const(f, cc);
        z->lhs->remove();
        z->lhs = new_use(xy->lhs->v, z);
        z->rhs->remove();
        z->rhs = new_use(c, z);
        return true;
    }

    if ((z->op_type == BINARY_ADD && xy->op_type == BINARY_SUBTRACT) ||
        (z->op_type == BINARY_SUBTRACT && xy->op_type == BINARY_ADD)) {
        int64 a = xyc->value;
        if (xy->op_type == BINARY_SUBTRACT)
            a = -a;

        int64 b = zc->value;
        if (z->op_type == BINARY_SUBTRACT)
            b = -b;

        int64 cc = a + b;

        z->op_type = BINARY_ADD;
        auto c = new_const(f, cc);
        if (cc < 0) {
            z->op_type = BINARY_SUBTRACT;
            c->value = -c->value;
        }

        z->lhs->remove();
        z->lhs = new_use(xy->lhs->v, z);
        z->rhs->remove();
        z->rhs = new_use(c, z);
        return true;
    }

    return false;
}

// n*2 + m*2 = （m+n)*2
// n/8 + m/8 = （m+n) / 8
bool combine_distr(Procedure_IR *f, Value *v) {

    return false;

    auto acc = v->as<Instruction_Binary>();
    if (!acc)
        return false;

    if (acc->op_type != BINARY_ADD && acc->op_type != BINARY_SUBTRACT)
        return false;

    auto l = acc->lhs->v->as<Instruction_Binary>();
    auto r = acc->rhs->v->as<Instruction_Binary>();
    if (!l)
        return false;
    if (!r)
        return false;

    if (l->op_type != r->op_type)
        return false;

    auto lc = l->rhs->v->as<Constant>();
    auto rc = r->lhs->v->as<Constant>();
    if (!lc)
        return false;
    if (!rc)
        return false;

    if (lc->value != rc->value)
        return false;
}

void inst_comb(Program_IR *prog) {
    for (auto f : prog->procedures) {
        GVN_GCM::move_constant_to_rhs(f);
        Queue<Value *> worklist;
        for (auto bb : f->blocks) {
            for (auto v : bb->insts) {
                worklist.push(v);
            }
        }

        while (!worklist.empty()) {
            auto v = worklist.front();
            worklist.pop();

            bool combined = false;

            combined |= combine_assoc(f, v);

            if (combined) {
                for (auto u = v->use; u; u = u->next) {
                    worklist.push(u->user);
                }
            }
        }

        dce_function(f);
    }
}
