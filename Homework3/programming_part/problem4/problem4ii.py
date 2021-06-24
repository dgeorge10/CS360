#!/usr/bin/python3

# LR(1) parsing algorithm
# Using the language of PLP calculator language

S = 'SHIFT'
R = 'REDUCE'
B = 'SHIFT-REDUCE'

class AR:
    """
    ActionRecord (AR) holds an action, and a state or production.
    """
    def __init__(self, action, stateprod):
        self.action = action
        self.newState = stateprod
        self.prod = stateprod

    def getAction(self):
        return self.action

    def getNewState(self):
        return self.newState

    def getProd(self):
        return self.prod

TABLE = [
        { "stmt_list": AR(S,2), "stmt": AR(B,3), "id": AR(S,3), "read": AR(S,1), "write": AR(S,4) },
        { "id": AR(B,5) },
        { "stmt": AR(B,2), "id": AR(S,3), "read": AR(S,1), "write": AR(S,4), "$$": AR(B,1) },
        { ":=": AR(S,5) },
        { "expr": AR(S,6), "term": AR(S,7), "factor": AR(B,9), "id": AR(B,12), "number": AR(B,13), "(": AR(S,8) },
        { "expr": AR(S,9), "term": AR(S,7), "factor": AR(B,9), "id": AR(B,12), "number": AR(B,13), "(": AR(S,8) },
        { "add_op": AR(S,10), "id": AR(R,6), "read": AR(R,6), "write": AR(R,6), "+": AR(B,14), "-": AR(B,15), 
            "$$": AR(R,6) },
        { "mult_op": AR(S,11), "id": AR(R,7), "read": AR(R,7), "write": AR(R,7), ")": AR(R,7), "+": AR(R,7),
            "-": AR(R,7), "*": AR(B,16), "/": AR(B,17), "$$": AR(R,7) },
        { "expr": AR(S,12), "term": AR(S,7), "factor": AR(B,9), "id": AR(B,12), "number": AR(B,13), "(": AR(S,8) },
        { "add_op": AR(S,10), "id": AR(R,4), "read": AR(R,4), "write": AR(R,4), "+": AR(B,14), "-": AR(B,15),
            "$$": AR(R,4) },
        { "term": AR(S,13), "factor": AR(B,9), "id": AR(B,12), "number": AR(B,13), "(": AR(S,8) },
        { "factor": AR(B,10), "id": AR(B,12), "number": AR(B,13), "(": AR(S,8) },
        { "add_op": AR(S,10), ")": AR(B,11), "+": AR(B,14), "-": AR(B,15) },
        { "mult_op": AR(S,11), "id": AR(R,8), "read": AR(R,8), "write": AR(R,8), ")": AR(R,8), "+": AR(R,8),
            "-": AR(R,8), "*": AR(B,16), "/": AR(B,17), "$$": AR(R,8) }
        ]

PRODUCTION = [
    ["program", "stmt_list $$"],
    ["stmt_list", "stmt_list stmt"],
    ["stmt_list", "stmt"],
    ["stmt", "id := expr"],
    ["stmt", "read id"],
    ["stmt", "write expr"],
    ["expr", "term"],
    ["expr", "expr add_op term"],
    ["term", "factor"],
    ["term", "term mult_op factor"],
    ["factor", "( expr )"],
    ["factor", "id"],
    ["factor", "number"],
    ["add_op", "+"],
    ["add_op", "-"],
    ["mult_op", "*"],
    ["mult_op", "/"]
    ]

TERMINALS = ["program", "stmt_list", "$$", "stmt", "id", ":=", "expr", "read", "write", "term", "add_op", "factor",
        "mult_op", "(", ")", "number", "+", "-", "*", "/"]

class Node:
    """
    Node class for holding satellite data in a tree.
    """
    def __init__(self, actualSymbol, symbol, state):
        self.actualSymbol = actualSymbol
        self.symbol = symbol
        self.state = state
        self.children = []

    def getSymbol(self):
        return self.symbol

    def getState(self):
        return self.state

    def addChild(self, child):
        self.children.append(child)

    def getChildren(self):
        return self.children

    def toString(self):
        out = self.actualSymbol
        if self.symbol != self.actualSymbol:
            out = self.symbol + "(" + out + ")"
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

def identify(token, tokens):
    if token in tokens:
        return token
    elif token.isnumeric():
        return "number"
    else:
        return "id"

def tableDrivenParser(input_str, table, production, terminals):
    """
    This method implements the table-driven SLR(1) parser in PLP Figure 2.29.
    I use a Python list as a stack here to hold nodes (each node's satellite data being
    a token), and build the tree while pushing and popping tokens onto the stack.

    Returns a parse tree if successful, returns nothing if there is an error.

    input_str : a string to run the parser on. Tokens separated by space.
    table : index by top of stack state, then symbol. A table of action records.
    production : a list of productions, each having left hand and right hand sides.
    terminals : list of defined symbols
    """
    startState = 0
    startSymbol = production[0][0]
    start = Node(None, None, startState) # Create root node
    stack = [ start ] # Start on first symbol (parser stack)
    i = 0
    tokens = input_str.split(' ')
    actualCurSym = tokens[i]
    # Build a stream for the tree, all terminals as leaves
    treeStream = []
    for token in tokens:
        treeStream.append(Node(token, identify(token, terminals), None))
    # Separate stack for the tree
    treeStack = []
    curSym = identify(actualCurSym, terminals) # Get new token from scanner
    while True:
        try:
            head = stack[-1]
            curState = head.getState()
            if curState == startState and curSym == startSymbol:
                return treeStream[0]
            ar = table[curState][curSym]
            if ar.action == S:
                newNode = Node(actualCurSym, curSym, ar.getNewState())
                stack.append(newNode)
                i += 1
                actualCurSym = tokens[i]
                curSym = identify(actualCurSym, terminals)
                # Move first element of stream into tree
                treeStack.append(treeStream.pop(0))
            elif ar.action == R:
                actualCurSym = production[ar.getProd() - 1][0]
                curSym = actualCurSym
                # Make a new tree for lhs
                newNode = Node(actualCurSym, curSym, None)
                children = []
                for j in range(len(production[ar.getProd() - 1][1].split(' '))):
                    stack.pop()
                    # Remove last n elements from tree stack...
                    children.append(treeStack.pop())
                # Insert into tree stream
                treeStream.insert(0, newNode)
                for child in reversed(children):
                    # Add the children to the reduced element
                    newNode.addChild(child)
                i -= 1
            elif ar.action == B:
                actualCurSym = production[ar.getProd() - 1][0]
                curSym = actualCurSym
                # Shift one element for end of children
                end = treeStream.pop(0)
                # Make a new tree for lhs
                newNode = Node(actualCurSym, curSym, None)
                children = []
                for j in range(len(production[ar.getProd() - 1][1].split(' ')) - 1):
                    stack.pop()
                    # Remove last n-1 elements from tree stack...
                    children.append(treeStack.pop())
                # Insert into tree stream
                treeStream.insert(0, newNode)
                for child in reversed(children):
                    # Add the children to the reduced element
                    newNode.addChild(child)
                # Add the shifted element
                newNode.addChild(end)
        except Exception as e:
            raise e

print("Test 1 (2.38)")
tree = tableDrivenParser('read A read B sum := A + B write sum write sum / 2 $$', TABLE, PRODUCTION, TERMINALS)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))

