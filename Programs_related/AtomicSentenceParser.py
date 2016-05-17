# -----------------------------------------------------------------------------
# A parser for simple first order logic sentence using David Beazley's PLY.
#
#
# -----------------------------------------------------------------------------
import ply.lex as lex


tokens = [
    'NAMES', 'LPAREN', 'RPAREN', 'COMMA', 'VARIABLES', 'CONSTANTS']

# Tokens
t_LPAREN       = r'\('
t_RPAREN       = r'\)'
t_COMMA        = r'\,'

#ignored characters
t_ignore = " \t"

def noupper(name):
    for i in range(len(name)):
        if name[i].isupper():
            return False
    return True

def nolower(name):
    for i in range(len(name)):
        if name[i].islower():
            return False
    return True

def t_NAMES(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if noupper(t.value) and t.value != 'not':
	t.type = 'VARIABLES'
    elif nolower(t.value):
	t.type = 'CONSTANTS'
    else:
	t.type = 'NAMES'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    
# Build the lexer
lexer = lex.lex()

# Build the tree for a logic sentence according to expression rule

class Node:
    def __init__(self,type,children=None,leaf=None):
         self.type = type
         if children:
              self.children = children
         else:
              self.children = [ ]
         self.leaf = leaf

precedence = (
    ('left', 'COMMA'),
    ('left', 'LPAREN', 'RPAREN')
    )

def p_expression_comma(p):
    'expression : expression COMMA expression'
    if isinstance(p[1], list) and isinstance(p[3], list):
	p[0] = p[1] + p[3]
    elif isinstance(p[1], list):
	p[0] = p[1] + [p[3]]
    elif isinstance(p[3], list):
	p[0] = [p[1]] + p[3]
    else:
	p[0] = [p[1]] + [p[3]]

def p_expression_paren(p):
    'expression : expression LPAREN expression RPAREN'
    if isinstance(p[3],list):
	p[0] = Node('PAREN', p[3], p[1])
    else:
	p[0] = Node('PAREN', [p[3]], p[1])

def p_expression_names(p):
    'expression : NAMES'
    p[0] = p[1]

def p_expression_variables(p):
    'expression : VARIABLES'
    p[0] = Node('VARIABLES', [], p[1])

def p_expression_constants(p):
    'expression : CONSTANTS'
    p[0] = Node('CONSTANTS', [], p[1])

def p_error(p):
    try:
        print("Syntax error at '%s'" % p.value)
    except:  
        print("Syntax error. An equation is expected.")

#build the yacc
import ply.yacc as yacc
yacc.yacc()


def atomicparse(s):
    return yacc.parse(s)

