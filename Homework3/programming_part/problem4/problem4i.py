#!/usr/bin/python3

TABLE = {
        'program': {'id': 1, 'read': 1, 'write': 1, '$$': 1},
        'stmt_list': {'id': 2, 'read': 2, 'write': 2, '$$': 3},
        'stmt': {'id': 4, 'read': 5, 'write': 6},
        'expr': {'id': 7, 'number': 7, '(': 7},
        'term_tail': {'id': 9, 'read': 9, 'write': 9, ')': 9, '+': 8, '-': 8, '$$': 9},
        'term': {'id': 10, 'number': 10, '(': 10},
        'factor_tail': {'id': 12, 'read': 12, 'write': 12, ')': 12, '+': 12, '-': 12,
            '*': 11, '/': 11, '$$': 12},
        'factor': {'id': 14, 'number': 15, '(': 13},
        'add_op': {'+': 16, '-': 17},
        'mult_op': {'*': 18, '/': 19},
}

GRAMMAR = [
        ('program', 'stmt_list $$'),
        ('stmt_list', 'stmt stmt_list'),
        ('stmt_list', ''),
        ('stmt', 'id := expr'),
        ('stmt', 'read id'),
        ('stmt', 'write expr'),
        ('expr', 'term term_tail'),
        ('term_tail', 'add_op term term_tail'),
        ('term_tail', ''),
        ('term', 'factor factor_tail'),
        ('factor_tail', 'mult_op factor factor_tail'),
        ('factor_tail', ''),
        ('factor', '( expr )'),
        ('factor', 'id'),
        ('factor', 'number'),
        ('add_op', '+'),
        ('add_op', '-'),
        ('mult_op', '*'),
        ('mult_op', '/'),
]

TOKENS = [ 'program', 'stmt_list', '$$', 'stmt', 'id', ':=', 'expr', 'read', 'write', 'term', 'term_tail',
        'add_op', 'factor', 'factor_tail', 'mult_op', '(', ')', 'id', 'number', '+', '-', '*', '/', '<EPSILON>' ]

def identify(token, tokens):
    if token in tokens:
        return token
    elif token.isnumeric():
        return "number"
    else:
        return "id"

class Node:
    """
    Node class for holding satellite data in a tree.
    """
    def __init__(self, symbol):
        self.symbol = symbol
        self.children = []

    def getSymbol(self):
        return self.symbol

    def setSymbol(self, sym):
        self.symbol = sym

    def addChild(self, child):
        self.children.append(child)

    def getChildren(self):
        return self.children

    def toString(self):
        out = identify(self.symbol, TOKENS)
        if self.symbol != out:
            out = out + "(" + self.symbol + ")"
        return out

def computeHeight(root):
    """
    Compute the height at the root node.
    """
    if len(root.getChildren()) == 0:
        return 0
    else:
        return 1 + max(map(lambda c: computeHeight(c), root.getChildren()))

def preorder(root):
    """
    List the nodes in preorder; i.e. print current node then children
    """
    order = root.toString()
    for child in root.getChildren():
        order += " " + preorder(child)
    return order

def postorder(root):
    """
    List the nodes in postorder; i.e. print children then current node
    """
    order = ""
    for child in root.getChildren():
        order += postorder(child) + " "
    order += root.toString()
    return order

def tableDrivenParser(input_str, table, grammar, tokens_list):
    """
    This method implements the table-driven parser in PLP Figure 2.19.
    I use a Python list as a stack here to hold nodes (each node's satellite data being
    a token), and build the tree while pushing and popping tokens onto the stack.

    Returns a parse tree if successful, returns nothing if there is an error.

    input_str : a string to run the parser on. Tokens separated by space.
    table : a mapping of stack symbol and lookahead to a production rule
    grammar : a list of production rules
    """
    root = Node(next(iter(table))) # Create root node
    stack = [ root ] # Start on first symbol
    i = 0
    terminals = input_str.split(' ')
    while i < len(terminals) and len(stack) > 0:
        # End of stack will represent top symbol
        topNode = stack[-1]
        top = topNode.getSymbol()
        lookahead = terminals[i]
        is_terminal = top not in table
        if is_terminal:
            if identify(lookahead, tokens_list) == top:
                # Move the matched lookahead off stack and input
                topNode.setSymbol(lookahead)
                stack.pop()
                i += 1
            else:
                # Top of stack is terminal that does not match input lookahead
                print("Error! Top of stack does not match lookahead symbol.")
                return
        else:
            # We have a symbol
            if identify(lookahead, tokens_list) not in table[top]:
                print("Error! Unable to construct parse tree.")
                return
            entry = table[top][identify(lookahead, tokens_list)]
            # Remove symbol, replace with production
            production = grammar[entry-1]
            if production[0] != top:
                print("Error! Incorrect production.")
                return
            # Remove topNode
            stack.pop()
            # Push production on to stack, connect nodes to topNode
            if production[1] == '': # Ignore epsilon. Epsilon = empty string.
                topNode.addChild(Node('<EPSILON>'))
                continue
            tokens = production[1].split(' ')
            nodes = []
            for t in tokens:
                newNode = Node(t)
                topNode.addChild(newNode)
                nodes.append(newNode)
            # Need to reverse the nodes now to push correctly on stack
            for n in reversed(nodes):
                stack.append(n)
    return root

print("Test 1 (2.24)")
tree = tableDrivenParser('read A read B sum := A + B write sum write sum / 2 $$', TABLE, GRAMMAR, TOKENS)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))

