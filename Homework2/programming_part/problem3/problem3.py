#!/usr/bin/python3

# Using the language of simple programming statements (FCS 11.33)

TABLE = {
        '<S>': {'w': 1, 'c': None, '{': 2, '}': None, 's': 3, ';': None, 'ENDM': None},
        '<T>': {'w': 4, 'c': None, '{': 4, '}': 5, 's': 4, ';': None, 'ENDM': None}
}

GRAMMAR = [
        ('<S>', 'w c <S>'),
        ('<S>', '{ <T>'),
        ('<S>', 's ;'),
        ('<T>', '<S> <T>'),
        ('<T>', '}')
]

class Node:
    """
    Node class for holding satellite data in a tree.
    """
    def __init__(self, symbol):
        self.symbol = symbol
        self.children = []

    def getSymbol(self):
        return self.symbol

    def addChild(self, child):
        self.children.append(child)

    def getChildren(self):
        return self.children

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
    order = root.getSymbol()
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
    order += root.getSymbol()
    return order

def tableDrivenParser(input_str, table, grammar):
    """
    This method implements the table-driven parser in FCS 11.17.
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
            if lookahead == top:
                # Move the matched lookahead off stack and input
                stack.pop()
                i += 1
            else:
                # Top of stack is terminal that does not match input lookahead
                print("Error! Top of stack does not match lookahead symbol.")
                return
        else:
            # We have a symbol
            entry = table[top][lookahead]
            if entry is None:
                print("Error! Unable to construct parse tree.")
                return
            # Remove symbol, replace with production
            production = grammar[entry-1]
            if production[0] != top:
                print("Error! Incorrect production.")
                return
            # Remove topNode
            stack.pop()
            # Push production on to stack, connect nodes to topNode
            if production[1] == '': # Ignore epsilon. Epsilon = empty string.
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

# Example 11.36 from FCS
print("Test 1 (11.36)")
tree = tableDrivenParser('{ w c s ; s ; } ENDM', TABLE, GRAMMAR)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))

# Example 11.7.1a from FCS
print("Test 2 (11.7.1a)")
tree = tableDrivenParser('{ s ; } ENDM', TABLE, GRAMMAR)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))

# Example 11.7.1b from FCS
print("Test 3 (11.7.1b)")
tree = tableDrivenParser('w c { s ; s ; } ENDM', TABLE, GRAMMAR)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))

# Example 11.7.1c from FCS
print("Test 4 (11.7.1c)")
tree = tableDrivenParser('{ { s ; s ; } s ; } ENDM', TABLE, GRAMMAR)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))

# Example 11.7.1d from FCS. This is an invalid input, intended to fail.
print("Test 5 (11.7.1d)")
tree = tableDrivenParser('{ s ; s } ENDM', TABLE, GRAMMAR)

PARENTHESES_TABLE = {
        '<B>': { '(': 2, ')': 1, 'ENDM': 1 }
}

PARENTHESES_GRAMMAR = [
        ('<B>', ''),
        ('<B>', '( <B> ) <B>')
]

# Simulate on 11.31 table
print("Test 6 (11.31)")
tree = tableDrivenParser('( ( ) ( ) ) ( ( ) ) ENDM', PARENTHESES_TABLE, PARENTHESES_GRAMMAR)
print("Height of tree = %d" % computeHeight(tree))
print("Preorder       = %s" % preorder(tree))
print("Postorder      = %s" % postorder(tree))


