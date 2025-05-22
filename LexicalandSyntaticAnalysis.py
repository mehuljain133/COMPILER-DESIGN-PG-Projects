# Unit- I Lexical and Syntactic Analysis: Review of regular languages, design of a lexical analyzer generator, context free grammars, syntactic analysis: top down parsing: recursive descent and predictive parsing, LL(k) parsing; bottom up parsing: LR parsing, handling ambiguous in bottom up parsers

import re
from collections import defaultdict, deque

class Lexer:
    def __init__(self, token_specs):
        self.token_specs = token_specs
        self.regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in token_specs)
        self.pattern = re.compile(self.regex)

    def tokenize(self, code):
        tokens = []
        for mo in self.pattern.finditer(code):
            kind = mo.lastgroup
            value = mo.group()
            if kind == 'SKIP':
                continue
            if kind == 'MISMATCH':
                raise RuntimeError(f'Illegal character {value!r}')
            tokens.append((kind, value))
        tokens.append(('$', '$'))
        return tokens

class Grammar:
    def __init__(self, productions):
        self.productions = productions
        self.nonterminals = list(productions.keys())
        self.terminals = set()
        self._find_terminals()
        self.start_symbol = self.nonterminals[0]

    def _find_terminals(self):
        nts = set(self.nonterminals)
        terms = set()
        for prods in self.productions.values():
            for prod in prods:
                for sym in prod:
                    if sym not in nts and sym != '':
                        terms.add(sym)
        self.terminals = terms

    def print_grammar(self):
        print("Grammar productions:")
        for nt in self.productions:
            for prod in self.productions[nt]:
                rhs = ' '.join(prod) if prod else "ε"
                print(f"  {nt} -> {rhs}")
        print()

def compute_first_sets(grammar):
    first = {nt: set() for nt in grammar.nonterminals}
    first.update({t: {t} for t in grammar.terminals})
    changed = True
    while changed:
        changed = False
        for nt in grammar.nonterminals:
            for prod in grammar.productions[nt]:
                i = 0
                add_epsilon = True
                while add_epsilon and i < len(prod):
                    sym = prod[i]
                    sym_first = first[sym] if sym in first else {sym}
                    before = len(first[nt])
                    first[nt].update(sym_first - {''})
                    after = len(first[nt])
                    if after > before:
                        changed = True
                    if '' in sym_first:
                        i += 1
                    else:
                        add_epsilon = False
                if add_epsilon:
                    if '' not in first[nt]:
                        first[nt].add('')
                        changed = True
    return first

def compute_follow_sets(grammar, first):
    follow = {nt: set() for nt in grammar.nonterminals}
    follow[grammar.start_symbol].add('$')
    changed = True
    while changed:
        changed = False
        for nt in grammar.nonterminals:
            for prod in grammar.productions[nt]:
                trailer = follow[nt].copy()
                for sym in reversed(prod):
                    if sym in grammar.nonterminals:
                        before = len(follow[sym])
                        follow[sym].update(trailer)
                        after = len(follow[sym])
                        if after > before:
                            changed = True
                        if '' in first[sym]:
                            trailer.update(first[sym] - {''})
                        else:
                            trailer = first[sym]
                    else:
                        trailer = first[sym] if sym in first else {sym}
    return follow

def compute_first_of_string(symbols, first):
    result = set()
    for sym in symbols:
        sym_first = first[sym] if sym in first else {sym}
        result.update(sym_first - {''})
        if '' not in sym_first:
            return result
    result.add('')
    return result

def construct_ll1_table(grammar, first, follow):
    table = defaultdict(dict)
    for nt in grammar.nonterminals:
        for prod in grammar.productions[nt]:
            first_prod = compute_first_of_string(prod, first)
            for terminal in first_prod - {''}:
                if terminal in table[nt]:
                    print(f"LL(1) Conflict at table[{nt}][{terminal}]")
                table[nt][terminal] = prod
            if '' in first_prod:
                for terminal in follow[nt]:
                    if terminal in table[nt]:
                        print(f"LL(1) Conflict at table[{nt}][{terminal}]")
                    table[nt][terminal] = prod
    return table

class LL1Parser:
    def __init__(self, grammar, table):
        self.grammar = grammar
        self.table = table

    def parse(self, tokens):
        stack = ['$']
        stack.append(self.grammar.start_symbol)
        pos = 0
        while stack:
            top = stack.pop()
            curr = tokens[pos][0]
            if top == '$' and curr == '$':
                return True
            if top in self.grammar.terminals or top == '$':
                if top == curr:
                    pos += 1
                else:
                    print(f"Error: expected {top} but got {curr}")
                    return False
            else:
                prod = self.table.get(top, {}).get(curr)
                if prod is None:
                    print(f"Error: no rule for {top} with lookahead {curr}")
                    return False
                if prod != ['']:
                    for sym in reversed(prod):
                        stack.append(sym)
        return False

class RecursiveDescentParser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def current(self):
        return self.tokens[self.pos][0]

    def match(self, t):
        if self.current() == t:
            self.pos += 1
            return True
        return False

    def parse(self):
        return self.E() and self.current() == '$'

    def E(self):
        if self.T():
            return self.E_prime()
        return False

    def E_prime(self):
        if self.match('PLUS'):
            if self.T():
                return self.E_prime()
            return False
        return True

    def T(self):
        if self.F():
            return self.T_prime()
        return False

    def T_prime(self):
        if self.match('MUL'):
            if self.F():
                return self.T_prime()
            return False
        return True

    def F(self):
        if self.match('LPAREN'):
            if self.E() and self.match('RPAREN'):
                return True
            return False
        elif self.match('ID') or self.match('NUMBER'):
            return True
        return False

class LR0Item:
    def __init__(self, lhs, rhs, dot=0):
        self.lhs = lhs
        self.rhs = rhs
        self.dot = dot

    def __eq__(self, other):
        return (self.lhs, self.rhs, self.dot) == (other.lhs, other.rhs, other.dot)

    def __hash__(self):
        return hash((self.lhs, tuple(self.rhs), self.dot))

    def __repr__(self):
        rhs_with_dot = self.rhs[:]
        rhs_with_dot.insert(self.dot, '•')
        rhs_str = ' '.join(rhs_with_dot) if rhs_with_dot else '•'
        return f"{self.lhs} -> {rhs_str}"

def closure(items, grammar):
    closure_set = set(items)
    changed = True
    while changed:
        changed = False
        new_items = set()
        for item in closure_set:
            if item.dot < len(item.rhs):
                sym = item.rhs[item.dot]
                if sym in grammar.nonterminals:
                    for prod in grammar.productions[sym]:
                        new_item = LR0Item(sym, prod, 0)
                        if new_item not in closure_set:
                            new_items.add(new_item)
        if new_items:
            closure_set.update(new_items)
            changed = True
    return closure_set

def goto(items, X, grammar):
    moved = set()
    for item in items:
        if item.dot < len(item.rhs) and item.rhs[item.dot] == X:
            moved.add(LR0Item(item.lhs, item.rhs, item.dot+1))
    return closure(moved, grammar)

def items(grammar):
    start_item = LR0Item(grammar.start_symbol + "'", [grammar.start_symbol], 0)
    C = []
    C.append(closure({start_item}, grammar))
    added = True
    while added:
        added = False
        for I in C:
            for X in list(grammar.nonterminals) + list(grammar.terminals):
                g = goto(I, X, grammar)
                if g and g not in C:
                    C.append(g)
                    added = True
    return C

def build_lr0_parsing_table(grammar):
    augmented_start = grammar.start_symbol + "'"
    grammar.productions[augmented_start] = [[grammar.start_symbol]]
    grammar.nonterminals.insert(0, augmented_start)
    C = items(grammar)

    states = {frozenset(state): idx for idx, state in enumerate(C)}

    ACTION = {}
    GOTO = {}

    for I in C:
        i = states[frozenset(I)]
        for item in I:
            if item.dot < len(item.rhs):
                a = item.rhs[item.dot]
                j = states.get(frozenset(goto(I, a, grammar)))
                if j is not None:
                    if a in grammar.terminals:
                        ACTION[(i,a)] = ('s', j)
                    else:
                        GOTO[(i,a)] = j
            else:
                if item.lhs == augmented_start:
                    ACTION[(i,'$')] = ('acc',)
                else:
                    for a in grammar.terminals.union({'$'}):
                        ACTION[(i,a)] = ('r', (item.lhs, item.rhs))
    return ACTION, GOTO, states

def lr0_parse(tokens, ACTION, GOTO):
    stack = [0]
    pos = 0
    while True:
        state = stack[-1]
        token = tokens[pos][0]
        action = ACTION.get((state, token))
        if action is None:
            print(f"Error: No action for state {state} and token {token}")
            return False
        if action[0] == 's':
            stack.append(action[1])
            pos += 1
        elif action[0] == 'r':
            lhs, rhs = action[1]
            for _ in rhs:
                stack.pop()
            state = stack[-1]
            stack.append(GOTO[(state, lhs)])
        elif action[0] == 'acc':
            return True

def ambiguous_grammar_example():
    print("""
Ambiguity Example: Dangling Else

Grammar:
S → if E then S | if E then S else S | other

Ambiguity:
The string "if E then if E then S else S" can be parsed in two ways:
- else pairs with the first if
- else pairs with the second if

Solutions include rewriting grammar or parser disambiguation rules.
""")

if __name__ == "__main__":
    token_specs = [
        ('NUMBER',  r'\d+(\.\d*)?'),
        ('ID',      r'[A-Za-z_]\w*'),
        ('PLUS',    r'\+'),
        ('MUL',     r'\*'),
        ('LPAREN',  r'\('),
        ('RPAREN',  r'\)'),
        ('SKIP',    r'[ \t\n]+'),
        ('MISMATCH',r'.'),
    ]

    lexer = Lexer(token_specs)

    code = "x + 3 * ( y + 2 )"

    print("Source code:", code)
    tokens = lexer.tokenize(code)
    print("Tokens:", tokens)

    productions = {
        'E': [['T','E\'']],
        'E\'': [['PLUS','T','E\''], ['']],
        'T': [['F','T\'']],
        'T\'': [['MUL','F','T\''], ['']],
        'F': [['LPAREN','E','RPAREN'], ['ID'], ['NUMBER']]
    }

    grammar = Grammar(productions)
    grammar.print_grammar()

    first = compute_first_sets(grammar)
    print("FIRST sets:")
    for nt in first:
        print(f"  FIRST({nt}) = {first[nt]}")
    print()

    follow = compute_follow_sets(grammar, first)
    print("FOLLOW sets:")
    for nt in follow:
        print(f"  FOLLOW({nt}) = {follow[nt]}")
    print()

    ll1_table = construct_ll1_table(grammar, first, follow)

    print("LL(1) Parsing Table (partial display):")
    for nt in ll1_table:
        print(f"{nt}:")
        for t in ll1_table[nt]:
            rhs = ' '.join(ll1_table[nt][t]) if ll1_table[nt][t] else 'ε'
            print(f"  {t} -> {rhs}")
    print()

    ll1_parser = LL1Parser(grammar, ll1_table)
    print("LL(1) Parser result:", "Accepted" if ll1_parser.parse(tokens) else "Rejected")
    print()

    rd_parser = RecursiveDescentParser(tokens)
    print("Recursive Descent Parser result:", "Accepted" if rd_parser.parse() else "Rejected")
    print()

    ACTION, GOTO, states = build_lr0_parsing_table(grammar)
    print(f"LR(0) Parsing states: {len(states)}")

    print("LR(0) Parsing result:", "Accepted" if lr0_parse(tokens, ACTION, GOTO) else "Rejected")
    print()

    ambiguous_grammar_example()
