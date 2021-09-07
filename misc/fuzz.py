import random

ident_count = 3

very_big_num = False

def roll(prob = 0.5):
    return random.random() < prob

def const(l = -100, r = 100):
    #return str(random.randint(-2147483648, 2147483647))
    rand_num =  random.randint(l, r)
    if very_big_num:
        if roll(0.8):
            rand_num = -2147483648
        elif roll(0.8):
            rand_num = 2147483647

    if rand_num < 0:
        return ' -' + str(-rand_num)
    else:
        return str(rand_num)

def ident(lval = False):
    global ident_count
    if (not lval) and roll(0.2):
        return 'C[{}]'.format(const(0, 3))
    return 'x' + str(random.choice(range(1, ident_count)))

def primary_expr():
    if roll(0.4):
        #return ident()
        return '(' + add_expr() + ')'
    elif roll():
        return ident()
    else:
        return const()

def unary_expr():
    if roll():
        return primary_expr()
    else:
        operand = const() if roll() else ident()
        op = random.choice([' ', ' -', ' +', ' !'])
        return op + operand;

def mul_expr():
    return unary_expr()
    if roll():
        return unary_expr()
    else:
        lhs = mul_expr()
        rhs = unary_expr()
        #op = random.choice(['*', '/', '%'])
        op = '*'

        return lhs + ' ' + op + ' ' + rhs;

def add_expr():
    if roll():
        return mul_expr()
    else:
        lhs = add_expr()
        rhs = mul_expr()
        op = random.choice(['+', '-'])

        return lhs + ' ' + op + ' ' + rhs;

def decl(init_value = None):
    global ident_count
    if init_value is None:
        init_value = add_expr()
    result = 'int x{} = {};'.format(ident_count, init_value)
    ident_count = ident_count+1
    return result

def assign():
    return '{} = {};'.format(ident(), add_expr())

def if_stmt(var, var2):
    cond = random.choice(['||', '&&'])
    #action = random.choice(['break', 'continue'])
    r = 'if ({} < {} {} {} < {}) {{\n'.format(var, const(5, 50), cond, var2, const(5, 50))
    if roll():
        r += 'break;\n' 
    else:
        r += '{} = {} + 5;\n'.format(var, var)
        r += '{} = {} + 5;\n'.format(var2, var2)
        r += 'continue;\n'
    r += '}\n'
    return r

def while_stmt():
    #r = decl(0)
    #var = 'x' + str(ident_count-1)
    var = ident(True)
    var2 = ident(True)
    cond = random.choice(['||', '&&'])
    r = 'while ({} < {} {} {} < {}) {{\n'.format(var, const(5, 50), cond, var2, const(5, 50))
    if roll(0.5):
        r += if_stmt(var, var2)
    if roll(0.5):
        r += while_stmt()
    if roll(0.2):
        r += while_stmt()
    if roll(0.1):
        r += while_stmt()
    if roll(0.5):
        #r += 'N = N + ' + var + ' + ' + add_expr() + ';\n'
        a = const(0, 4)
        r += 'A[{}] = A[{}] + A[{}] + {} + {}*2 + {};\n'.format(a, a, const(0, 4), var, var2, add_expr())
    r += '{} = {} + 1;\n'.format(var, var)
    r += '{} = {} + 1;\n'.format(var2, var2)
    r += '}\n'
    return r

def stmt():
    """
    if roll(1/8):
        return assign()
    elif roll(1/8):
        return add_expr() + ';'
    elif roll(1/8):
        return block()
    elif roll(1/8):
        return while_stmt()
    elif roll(1/8):
        return while_stmt()
    elif roll(1/8):
        return 'break;'
    elif roll(1/8):
        return 'continue;'
    else:
        return 'return ' + add_expr() + ';'
    """


def block(first = False):
    items = []
    if first:
        items.append('int x1 = 1;')
        items.append('int x2 = 2;')
    for x in range(10):
        if roll():
            items.append(decl())
        else:
            items.append(while_stmt())
    if first:
        items.append('putarray(5, A); putch(10);')
        items.append('return 0;')
    return '{\n\t' + '\n\t'.join(items) + '\n}\n'


print('int A[5];\nconst int C[4]={1,2,3,4};\nint main() ' + block(True))
