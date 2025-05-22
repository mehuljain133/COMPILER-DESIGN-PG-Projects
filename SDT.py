# Unit-II Syntax directed translation: Top down and bottom up approaches, data types, mixed mode expression; subscripted variables, sequencing statement, subroutines and functions: parameters calling, subroutines with side effects.

import re

# --------- Lexer ---------

class Lexer:
    def __init__(self):
        self.rules = [
            ('NUMBER',   r'\d+(\.\d+)?'),      # integer or float
            ('ID',       r'[A-Za-z_]\w*'),     # identifiers
            ('ASSIGN',   r'='),                # assignment
            ('PLUS',     r'\+'),
            ('MINUS',    r'-'),
            ('MUL',      r'\*'),
            ('DIV',      r'/'),
            ('LPAREN',   r'\('),
            ('RPAREN',   r'\)'),
            ('LBRACK',   r'\['),
            ('RBRACK',   r'\]'),
            ('COMMA',    r','),
            ('SEMICOLON',r';'),
            ('FUNC',     r'func'),
            ('RETURN',   r'return'),
            ('SKIP',     r'[ \t\n]+'),
            ('MISMATCH', r'.'),
        ]
        self.regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in self.rules)
        self.pattern = re.compile(self.regex)

    def tokenize(self, text):
        tokens = []
        for match in self.pattern.finditer(text):
            kind = match.lastgroup
            value = match.group()
            if kind == 'SKIP':
                continue
            elif kind == 'MISMATCH':
                raise RuntimeError(f'Unexpected character {value}')
            tokens.append((kind, value))
        tokens.append(('EOF', 'EOF'))
        return tokens

# --------- AST Nodes ---------

class Number:
    def __init__(self, val):
        self.value = float(val) if '.' in val else int(val)
    def __repr__(self): return f"Number({self.value})"

class Variable:
    def __init__(self, name, index=None):
        self.name = name
        self.index = index  # None if scalar
    def __repr__(self):
        if self.index is not None:
            return f"{self.name}[{self.index}]"
        return self.name

class BinOp:
    def __init__(self, left, op, right):
        self.left, self.op, self.right = left, op, right
    def __repr__(self): return f"({self.left} {self.op} {self.right})"

class Assignment:
    def __init__(self, var, expr):
        self.var, self.expr = var, expr
    def __repr__(self): return f"{self.var} = {self.expr}"

class Sequence:
    def __init__(self, stmts):
        self.stmts = stmts
    def __repr__(self): return '\n'.join(map(str, self.stmts))

class FuncDef:
    def __init__(self, name, params, body):
        self.name, self.params, self.body = name, params, body
    def __repr__(self): return f"func {self.name}({', '.join(self.params)}) {{\n{self.body}\n}}"

class FuncCall:
    def __init__(self, name, args):
        self.name, self.args = name, args
    def __repr__(self): return f"{self.name}({', '.join(map(str, self.args))})"

class Return:
    def __init__(self, expr):
        self.expr = expr
    def __repr__(self): return f"return {self.expr}"

# --------- Parser ---------

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.current = tokens[0]

    def error(self, msg='Syntax error'):
        raise Exception(msg)

    def eat(self, kind):
        if self.current[0] == kind:
            val = self.current[1]
            self.pos += 1
            if self.pos < len(self.tokens):
                self.current = self.tokens[self.pos]
            return val
        self.error(f"Expected {kind} but got {self.current}")

    def parse(self):
        stmts = []
        while self.current[0] != 'EOF':
            stmts.append(self.statement())
        return Sequence(stmts)

    def statement(self):
        if self.current[0] == 'FUNC':
            return self.funcdef()
        elif self.current[0] == 'RETURN':
            return self.returnstmt()
        elif self.current[0] == 'ID':
            # could be assignment or function call
            next_tok = self.tokens[self.pos+1] if self.pos+1 < len(self.tokens) else ('EOF','EOF')
            if next_tok[0] == 'ASSIGN' or next_tok[0] == 'LBRACK':
                return self.assignment()
            else:
                fc = self.funccall()
                self.eat('SEMICOLON')
                return fc
        else:
            self.error("Invalid statement")

    def funcdef(self):
        self.eat('FUNC')
        name = self.eat('ID')
        self.eat('LPAREN')
        params = []
        if self.current[0] == 'ID':
            params.append(self.eat('ID'))
            while self.current[0] == 'COMMA':
                self.eat('COMMA')
                params.append(self.eat('ID'))
        self.eat('RPAREN')
        self.eat('LBRACK')
        body = []
        while self.current[0] not in ('RETURN', 'RBRACK'):
            body.append(self.statement())
        ret = self.returnstmt()
        self.eat('RBRACK')
        return FuncDef(name, params, Sequence(body + [ret]))

    def returnstmt(self):
        self.eat('RETURN')
        expr = self.expr()
        self.eat('SEMICOLON')
        return Return(expr)

    def assignment(self):
        var = self.variable()
        self.eat('ASSIGN')
        expr = self.expr()
        self.eat('SEMICOLON')
        return Assignment(var, expr)

    def variable(self):
        name = self.eat('ID')
        if self.current[0] == 'LBRACK':
            self.eat('LBRACK')
            index = self.expr()
            self.eat('RBRACK')
            return Variable(name, index)
        return Variable(name)

    def funccall(self):
        name = self.eat('ID')
        self.eat('LPAREN')
        args = []
        if self.current[0] != 'RPAREN':
            args.append(self.expr())
            while self.current[0] == 'COMMA':
                self.eat('COMMA')
                args.append(self.expr())
        self.eat('RPAREN')
        return FuncCall(name, args)

    def expr(self):
        node = self.term()
        while self.current[0] in ('PLUS', 'MINUS'):
            op = self.eat(self.current[0])
            node = BinOp(node, op, self.term())
        return node

    def term(self):
        node = self.factor()
        while self.current[0] in ('MUL', 'DIV'):
            op = self.eat(self.current[0])
            node = BinOp(node, op, self.factor())
        return node

    def factor(self):
        tok = self.current
        if tok[0] == 'NUMBER':
            self.eat('NUMBER')
            return Number(tok[1])
        elif tok[0] == 'ID':
            # function call or variable
            next_tok = self.tokens[self.pos+1] if self.pos+1 < len(self.tokens) else ('EOF', 'EOF')
            if next_tok[0] == 'LPAREN':
                return self.funccall()
            else:
                return self.variable()
        elif tok[0] == 'LPAREN':
            self.eat('LPAREN')
            node = self.expr()
            self.eat('RPAREN')
            return node
        else:
            self.error('Invalid factor')

# --------- Interpreter ---------

class Environment:
    def __init__(self, parent=None):
        self.vars = {}
        self.arrays = {}
        self.parent = parent

    def get(self, name):
        if name in self.vars:
            return self.vars[name]
        if self.parent:
            return self.parent.get(name)
        raise Exception(f"Variable '{name}' not found")

    def set(self, name, value):
        self.vars[name] = value

    def get_array(self, name):
        if name in self.arrays:
            return self.arrays[name]
        if self.parent:
            return self.parent.get_array(name)
        raise Exception(f"Array '{name}' not found")

    def set_array(self, name, arr):
        self.arrays[name] = arr

class Interpreter:
    def __init__(self, tree):
        self.tree = tree
        self.global_env = Environment()
        self.functions = {}

    def visit(self, node, env):
        method = 'visit_' + type(node).__name__
        return getattr(self, method)(node, env)

    def visit_Sequence(self, node, env):
        for stmt in node.stmts:
            res = self.visit(stmt, env)
            if isinstance(res, Return):
                return res
        return None

    def visit_Number(self, node, env):
        return node.value

    def visit_Variable(self, node, env):
        if node.index is None:
            return env.get(node.name)
        else:
            idx = self.visit(node.index, env)
            arr = env.get_array(node.name)
            return arr[int(idx)]

    def visit_BinOp(self, node, env):
        l = self.visit(node.left, env)
        r = self.visit(node.right, env)
        # Mixed mode: cast int to float if necessary
        if isinstance(l, int) and isinstance(r, float): l = float(l)
        if isinstance(r, int) and isinstance(l, float): r = float(r)
        if node.op == '+': return l + r
        if node.op == '-': return l - r
        if node.op == '*': return l * r
        if node.op == '/': return l / r

    def visit_Assignment(self, node, env):
        val = self.visit(node.expr, env)
        if node.var.index is None:
            env.set(node.var.name, val)
        else:
            idx = self.visit(node.var.index, env)
            arr = env.get_array(node.var.name)
            arr[int(idx)] = val

    def visit_FuncDef(self, node, env):
        self.functions[node.name] = node

    def visit_FuncCall(self, node, env):
        if node.name not in self.functions:
            raise Exception(f"Function '{node.name}' not defined")
        fdef = self.functions[node.name]
        if len(fdef.params) != len(node.args):
            raise Exception(f"Function '{node.name}' expects {len(fdef.params)} arguments")
        new_env = Environment(self.global_env)
        for param, arg in zip(fdef.params, node.args):
            new_env.set(param, self.visit(arg, env))
        ret = self.visit(fdef.body, new_env)
        if isinstance(ret, Return):
            return self.visit(ret.expr, new_env)
        return None

    def visit_Return(self, node, env):
        return node

# --------- Demo program ---------

code = """
x = 10;
y = 3.5;
arr[0] = 5;
arr[1] = 7;

func add(a, b) [
    result = a + b;
    return result;
]

z = add(x, y);
arr[1] = add(arr[0], z);
"""

lexer = Lexer()
tokens = lexer.tokenize(code)
parser = Parser(tokens)
ast = parser.parse()

interpreter = Interpreter(ast)
interpreter.global_env.set_array('arr', [0, 0])  # Initialize array

# Run
for stmt in ast.stmts:
    if isinstance(stmt, FuncDef):
        interpreter.visit(stmt, interpreter.global_env)
    else:
        interpreter.visit(stmt, interpreter.global_env)

print(f"x = {interpreter.global_env.get('x')}")
print(f"y = {interpreter.global_env.get('y')}")
print(f"z = {interpreter.global_env.get('z')}")
print(f"arr = {interpreter.global_env.get_array('arr')}")
