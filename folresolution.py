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

#This function will find the 'implies' in a tree for a logic sentence in order to remove them. The pointer to the 'implies' and the layer regarding the tree which the 'implies' is in will be recorded.
def FindImplies(Tree):
    global ImpliesPosition, layer
    layer += 1
    if Tree.leaf == '=>':
        ImpliesPosition.append([Tree, layer])
    if type(Tree.children) is list and len(Tree.children) == 2:
        FindImplies(Tree.children[0])
        FindImplies(Tree.children[1])
    elif Tree.children != []:
        FindImplies(Tree.children)

#This function will apply no to a subsentence as the result of remove 'implies'. Generally, universial quantifier will be changed to existential quantifier, 'and' to 'or' and vice versa. Atomic sentences will be applied on a not function.
def ApplyNot(SubTree):
    if 'Exist' in SubTree.leaf:
        SubTree.leaf = SubTree.leaf.replace('Exist', 'ForAll')
    elif 'ForAll' in SubTree.leaf:
        SubTree.leaf = SubTree.leaf.replace('ForAll', 'Exist')
    elif SubTree.leaf == '&':
        SubTree.leaf = '|'
    elif SubTree.leaf == '|':
        SubTree.leaf = '&'
    elif 'not' in SubTree.leaf:
        SubTree.leaf = SubTree.leaf.replace('not(', '').replace(')', '')+')'
    else:
        SubTree.leaf = 'not(' + SubTree.leaf +')'    
    if type(SubTree.children) is list and len(SubTree.children) == 2:
        ApplyNot(SubTree.children[0])
        ApplyNot(SubTree.children[1])
    elif SubTree.children != []:
        ApplyNot(SubTree.children)

#This function find all the variables which is defined by quantifiers in a sentence. This will help the variable standardization process later.
def FindOVariables(Tree):
    global OldVariables
    if 'Exist' in Tree.leaf or 'ForAll' in Tree.leaf:
        OldVariables.append(Tree.leaf.replace('Exist{', '').replace('ForAll{', '').replace('}', '').split(',')[0])
    if type(Tree.children) is list and len(Tree.children) == 2:
        FindOVariables(Tree.children[0])
        FindOVariables(Tree.children[1])
    elif Tree.children != []:
        FindOVariables(Tree.children)

#The following two functions will finish the variable standardization process by assign quantifiers new variables in a reserved variable list. After that, all the variables within the scope of this quantifier should be changed by the ReplaceVariables function.
def AssignNewQuantifiers(Tree):
    global Variables, AssignOrder
    if 'Exist' in Tree.leaf:
        OVarlist = Tree.leaf.replace('Exist{', '').replace('}', '').split(',')
        for i in range(len(OVarlist)):
	    if i == 0:
                NVarlist = Variables[AssignOrder]
	    else:
    		NVarlist = NVarlist + ',' + Variables[AssignOrder]
	    AssignOrder += 1
        Tree.leaf = 'Exist{' + NVarlist + '}'
        for i in range(len(OVarlist)):
            ReplaceVariables(Tree, OVarlist[i], NVarlist.split(',')[i])
    if 'ForAll' in Tree.leaf:
        OVarlist = Tree.leaf.replace('ForAll{', '').replace('}', '').split(',')
        for i in range(len(OVarlist)):
	    if i == 0:
                NVarlist = Variables[AssignOrder]
	    else:
    		NVarlist = NVarlist + ',' + Variables[AssignOrder]
	    AssignOrder += 1
        Tree.leaf = 'ForAll{' + NVarlist + '}'
        for i in range(len(OVarlist)):
            ReplaceVariables(Tree, OVarlist[i], NVarlist.split(',')[i])
    if type(Tree.children) is list and len(Tree.children) == 2:
        AssignNewQuantifiers(Tree.children[0])
        AssignNewQuantifiers(Tree.children[1])
    elif Tree.children != []:
        AssignNewQuantifiers(Tree.children)

def ReplaceVariables(SubTree, OVar, NVar):
    if '(' in SubTree.leaf and '{' not in SubTree.leaf:
        j = SubTree.leaf.count('(')
        OVarlist = SubTree.leaf.split('(', 1)[1].replace(')', '').split(',')
        for i in range(len(OVarlist)):
            if OVarlist[i] == OVar:
		OVarlist[i] = NVar
        for i in range(len(OVarlist)):
            if i == 0:
                NVarlist = OVarlist[i]
	    else:
                NVarlist = NVarlist + ',' + OVarlist[i]
	SubTree.leaf = SubTree.leaf.split('(', 1)[0] + '(' + NVarlist + ')'*j
    if type(SubTree.children) is list and len(SubTree.children) == 2:
        ReplaceVariables(SubTree.children[0], OVar, NVar)
        ReplaceVariables(SubTree.children[1], OVar, NVar)
    elif SubTree.children != []:
        ReplaceVariables(SubTree.children, OVar, NVar)

#This function will replace variables referred by existential quantifier by Skolem functions. General rules is described in the corresponding part in the main function.
def AssignSkolemFunctions(Tree, UniversialVar):
    global FunctionAssignOrder, SkolemFunctions
    if 'ForAll' in Tree.leaf:
	NUniversialVar = Tree.leaf.replace('ForAll{', '').replace('}', '').split(',')
	for i in range(len(NUniversialVar)):
	    UniversialVar.append(NUniversialVar[i])
    if 'Exist' in Tree.leaf:
        OVarlist = Tree.leaf.replace('Exist{', '').replace('}', '').split(',')
	UniversialVarStr = ''
	for i in range(len(UniversialVar)):
	    if i == 0:
    		UniversialVarStr = UniversialVar[i]
	    else:
		UniversialVarStr = ',' + UniversialVar[i] 
        if len(UniversialVar) != 0:
	    for i in range(len(OVarlist)):
	        if i == 0:
                    NVarlist = SkolemFunctions[FunctionAssignOrder] + '(' + UniversialVarStr +')'
	        else:
    		    NVarlist = NVarlist + ',' + SkolemFunctions[FunctionAssignOrder] + '(' + UniversialVarStr + ')'
	        FunctionAssignOrder += 1
	else:
	    for i in range(len(OVarlist)):
	        if i == 0:
                    NVarlist = SkolemFunctions[FunctionAssignOrder] + '(' + OVarlist[i] + ')'
	        else:
    		    NVarlist = NVarlist + ',' + SkolemFunctions[FunctionAssignOrder] + '(' + OVarlist[i] + ')'
	        FunctionAssignOrder += 1
        Tree.leaf = 'Exist{' + NVarlist + '}'
        for i in range(len(OVarlist)):
            ReplaceVariables(Tree, OVarlist[i], NVarlist.split(',')[i])
    if type(Tree.children) is list and len(Tree.children) == 2:
        AssignSkolemFunctions(Tree.children[0], UniversialVar)
        AssignSkolemFunctions(Tree.children[1], UniversialVar)
    elif Tree.children != []:
        AssignSkolemFunctions(Tree.children, UniversialVar)

#This function will remove all the Quantifiers and return a new tree with all the quantifiers pruned.
def RemoveQuantifiers(Tree):
    if Tree == []:
        return []
    if type(Tree.children) is list and len(Tree.children) == 2:
        NNode = Node(Tree.type, [RemoveQuantifiers(Tree.children[0]), RemoveQuantifiers(Tree.children[1])], Tree.leaf)
	return NNode
    elif 'Exist' not in Tree.leaf and 'ForAll' not in Tree.leaf:
        NNode = Node(Tree.type, RemoveQuantifiers(Tree.children), Tree.leaf)
	return NNode
    else:
        return RemoveQuantifiers(Tree.children)

#This function will locate all the 'or's in the tree. The pointers and the layer of the 'or's in the tree will be recorded.
def FindOrs(Tree):
    global OrsPosition, layer
    layer += 1
    if Tree.leaf == '|':
        OrsPosition.append([Tree, layer])
    if type(Tree.children) is list and len(Tree.children) == 2:
        FindOrs(Tree.children[0])
        FindOrs(Tree.children[1])
    elif Tree.children != []:
        FindOrs(Tree.children)

#This function will distribute 'or' and 'and' to form a CNF sentence. General rules are described in the corresponding part in the main function
def DistributeOrAnd(SubTree):
    if SubTree != []:
        if SubTree.children[0].leaf == '&':
            SubTree.leaf = '&'
	    SubTree.children[1] = Node('OR', [SubTree.children[0].children[1], SubTree.children[1]], '|')
            SubTree.children[0].leaf = '|'
	    SubTree.children[0].children[1] = SubTree.children[1].children[1]
            DistributeOrAnd(SubTree.children[0])
        if SubTree.children[1].leaf == '&':
            SubTree.leaf = '&'
	    SubTree.children[0] = Node('OR', [SubTree.children[0], SubTree.children[1].children[0]], '|')
            SubTree.children[1].leaf = '|'
	    SubTree.children[1].children[0] = SubTree.children[0].children[0]
            DistributeOrAnd(SubTree.children[1])
    
# The following two functions will store all the sentence in the knowledge base into a 2D list with the first dimension indentifies different clauses.
def SeparateClauses(Tree):
    global NumberofClauses, SentenceArray
    if Tree.leaf == '|' or Tree.type == 'ATOMICSENTENCE':
        NumberofClauses += 1
        SentenceArray.append([])
        AddNewClause(Tree)
    else:
	SeparateClauses(Tree.children[0])
	SeparateClauses(Tree.children[1])

def AddNewClause(SubTree):
    global SentenceArray
    if SubTree.type == 'ATOMICSENTENCE':
        SentenceArray[NumberofClauses].append(SubTree.leaf)
    else:
        AddNewClause(SubTree.children[0])
	AddNewClause(SubTree.children[1])
    
    
#This part parse the input sentences of a knowledge base into trees
data = 'ForAll{x} [ForAll{y} Animal(y) => Loves(x,y)] => [Exist{y} Loves(y,x)]'
tree = parse(data)

#This part find the position of implies '=>' in every sentence and sort them according to the depth in the tree. Then remove them according to the rule 'A => B' equals 'not(A) | B'
layer = 0
ImpliesPosition = [[tree, -1]]
FindImplies(tree)
ImpliesPosition.sort(key = lambda x: int(x[1]), reverse = True)
for i in range(len(ImpliesPosition)-1):
    ImpliesPosition[i][0].leaf = '|'
    ApplyNot(ImpliesPosition[i][0].children[0])

#This part apply variable standardization to each sentence to reduce confusion before we drop the quantifiers. Every quantifier is in charge of all the variables to the right of it in its group.
Variables = ['x', 'y', 'z', 'a', 'b', 'c', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w']
OldVariables = []
FindOVariables(tree)
#All the old variables are picked out and abondoned. Later they will be replaced by new variables. This is a simple way to avoid conflict when we replace them.
for i in range(len(OldVariables)):
    if OldVariables[i] in Variables:
        Variables.remove(OldVariables[i])
AssignOrder = 0
AssignNewQuantifiers(tree)

#This part apply skolemization to existial quantifiers. The rule is that the arguments of the Skolem function are all the universally quantified variables in whose scope the existential quantifier appears.
SkolemFunctions = ['F', 'G', 'H', 'I', 'J', 'K'] 
FunctionAssignOrder = 0
UniversialVar = []
AssignSkolemFunctions(tree, UniversialVar)

#This part will remove all the quantifiers by making a copy of the original tree ignoring all the quantifiers.
NewTree = RemoveQuantifiers(tree)
tree = NewTree

#This part will distribute the 'or' and 'and' function to form a CNF. General rule is like '(A and B) or C' results in '(A or C) and (B or C)'. My strategy is to find all the 'or's and move them to the bottom of the tree until there is no 'and' blow it, starting from the 'or' with the maximum depth.
layer = 0
OrsPosition = [[tree, -1]]
FindOrs(tree)
OrsPosition.sort(key = lambda x: int(x[1]), reverse = True)
for i in range(len(OrsPosition)-1):
    DistributeOrAnd(OrsPosition[i][0])

#This part will separate each clauses in CNF and store them in a 2D list. The first dimension of the list indicates different clauses. Single Elements are all atomic sentences. For example, if the sentence is [A or B] & [C or D or E], SentenceArray = [[A, B], [C, D, E]].
SentenceArray = [[]]
NumberofClauses = 0
SeparateClauses(tree)

print SentenceArray
