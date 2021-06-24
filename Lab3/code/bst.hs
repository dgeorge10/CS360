data Tree a = 
    Empty 
    | Branch a (Tree a) (Tree a) 
    deriving (Show)


insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Branch x Empty Empty
insert x (Branch a left right)
    | x == a = Branch a left right
    | x < a = Branch a (insert x left) right
    | x > a = Branch a left (insert x right)


member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Branch a left right)
    | x == a = True
    | x < a = member x left 
    | x > a = member x right 

height :: Tree a -> Int 
height Empty = -1
height (Branch _ left right) = 1 + max (height left) (height right)

main = do
    let l1 = insert 4 Empty
    let l2 = insert 2 l1
    let l3 = insert 1 l2
    let l4 = insert 3 l3
    let l5 = insert 6 l4
    let l6 = insert 5 l5
    let bst = insert 7 l6
    print("bst:")
    print(bst)
    print("Testing member 1 bst")
    print(member 1 bst)
    print("Testing member 2 bst")
    print(member 2 bst)
    print("Testing member 3 bst")
    print(member 3 bst)
    print("Testing member 4 bst")
    print(member 4 bst)
    print("Testing member 5 bst")
    print(member 5 bst)
    print("Testing member 6 bst")
    print(member 6 bst)
    print("Testing member 7 bst")
    print(member 7 bst)
    print("Testing member 8 bst")
    print(member 8 bst)
    print("Testing member 0 bst")
    print(member 0 bst)
    print("height bst")
    print(height bst)
