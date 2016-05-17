# -----------------------------------------------------------------------------
# A parser for simple first order logic sentence using David Beazley's PLY.
#
#
# -----------------------------------------------------------------------------
import ply.lex as lex


tokens = [
    'AND','OR',
    'IMPLIES','IFANDONLYIF',
    'LPAREN','RPAREN','QUANTIFIER','ATOMICSENTENCE']

# Tokens
t_AND          = r'\&'
t_OR           = r'\|'
t_IMPLIES      = r'\=>'
t_IFANDONLYIF  = r'\<=>'
t_LPAREN       = r'\['
t_RPAREN       = r'\]'

# Ignored characters
t_ignore = " \t"
    
def t_QUANTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9\(\)\{\}\,]*'
    if '{' in t.value:
        t.type = 'QUANTIFIER'
    else:
        t.type = 'ATOMICSENTENCE'
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
precedence = (
    ('left', 'AND', 'OR', 'IMPLIES', 'IFANDONLYIF'),
    )

class Node:
    def __init__(self,type,children=None,leaf=None):
         self.type = type
         if children:
              self.children = children
         else:
              self.children = [ ]
         self.leaf = leaf


def p_expression_quantifier(p):
    'expression : QUANTIFIER expression'
    p[0] = Node('QUANTIFIER', p[2], p[1])

def p_expression_operator(p):
    '''expression : expression AND expression
                 | expression OR expression
                 | expression IMPLIES expression
                 | expression IFANDONLYIF expression'''
    p[0] = Node('OPERATOR', [p[1], p[3]], p[2])

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_atomicsentence(p):
    'expression : ATOMICSENTENCE'
    p[0] = Node('ATOMICSENTENCE', [], p[1])

def p_error(p):
    try:
        print("Syntax error at '%s'" % p.value)
    except:  
        print("Syntax error. An equation is expected.")

#build the yacc
import ply.yacc as yacc
yacc.yacc()


def parse(s):
    return yacc.parse(s)

