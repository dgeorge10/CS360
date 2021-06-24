{-# OPTIONS_GHC -fwarn-tabs #-}

module Huffman where

data Tree = Leaf Char Int | Branch Tree Tree [Char] Int

-- A straight port of the Huffman encoding algorithm in Week 3 Part 2 slides

makeLeaf :: Char -> Int -> Tree
makeLeaf symbol weight = Leaf symbol weight

isLeaf :: Tree -> Bool
isLeaf (Leaf _ _) = True
isLeaf (Branch _ _ _ _) = False

symbolLeaf :: Tree -> Char
symbolLeaf (Leaf s _) = s
symbolLeaf (Branch _ _ _ _) = error "Not a leaf"

weightLeaf :: Tree -> Int
weightLeaf (Leaf _ w) = w
weightLeaf (Branch _ _ _ _) = error "Not a leaf"

leftBranch :: Tree -> Tree
leftBranch (Leaf _ _) = error "Not a branch"
leftBranch (Branch l _ _ _) = l

rightBranch :: Tree -> Tree
rightBranch (Leaf _ _) = error "Not a branch"
rightBranch (Branch _ r _ _) = r

symbols :: Tree -> [Char]
symbols (Leaf s w) = [(symbolLeaf (Leaf s w))]
symbols (Branch _ _ s _) = s

weight :: Tree -> Int
weight (Leaf s w) = weightLeaf (Leaf s w)
weight (Branch _ _ _ w) = w

makeCodeTree :: Tree -> Tree -> Tree
makeCodeTree left right = Branch left right ((symbols left) ++ (symbols right)) ((weight left) + (weight right))

chooseBranch :: Int -> Tree -> Tree
chooseBranch b t
    | b == 0 = leftBranch t
    | b == 1 = rightBranch t
    | otherwise = error "bad bit -- CHOOSE-BRANCH"

decode :: [Int] -> Tree -> [Char]
decode message tree = decode1 message tree
    where decode1 bits currentBranch
              | length bits == 0 = []
              | otherwise = let nextBranch = chooseBranch (head bits) currentBranch
                            in if (isLeaf nextBranch) then ((symbolLeaf nextBranch) : (decode1 (tail bits) tree)) else (decode1 (tail bits) nextBranch)

containsSymbol :: Char -> Tree -> Bool
containsSymbol c (Leaf s _) = c == s
containsSymbol c (Branch _ _ s _) = c `elem` s

encodeSymbol :: Char -> Tree -> [Int]
encodeSymbol c (Leaf s _)
    | c == s = []
    | otherwise = error "Symbol not in tree"
encodeSymbol c (Branch l r s _)
    | containsSymbol c l = (0 : encodeSymbol c l)
    | containsSymbol c r = (1 : encodeSymbol c r)
    | otherwise = error "Symbol not in tree"

encode :: [Char] -> Tree -> [Int]
encode [] _ = []
encode (c:msg) t = ((encodeSymbol c t) ++ (encode msg t))

main = do
    -- Part 1 from Week 3 files
    let tree1 = makeCodeTree (makeLeaf 'a' 7) (makeCodeTree (makeCodeTree (makeLeaf 'b' 9) (makeLeaf 'c' 12)) (makeLeaf 'd' 22))
    -- Example of encoding
    let msg1 = "bcda"
    print "Original message:"
    print msg1
    let enc1 = encode msg1 tree1
    print "Encoded message:"
    print enc1
    -- Example of decoding
    let msg2 = [1, 0, 0, 1, 0, 1, 1, 1, 0]
    print "Expected encoded message:"
    print msg2
    let dec1 = decode msg2 tree1
    print "Decoded message:"
    print dec1
    -- Part 2 from Week 3 files
    let tree2 = makeCodeTree (makeCodeTree (makeLeaf 'd' 22) (makeLeaf 'e' 23)) 
                                           (makeCodeTree (makeLeaf 'f' 27) 
                                                         (makeCodeTree (makeLeaf 'c' 12) 
                                                         (makeCodeTree (makeLeaf 'a' 7) (makeLeaf 'b' 9))))
    -- More exmaples of encoding and decoding
    let msg3 = "abcdef"
    print "Original message:"
    print msg3
    let enc2 = encode msg3 tree2
    print "Encoded message:"
    print enc2
    let msg4 = [1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0]
    print "Expected encoded message:"
    print msg4
    let dec2 = decode msg4 tree2
    print "Decoded message:"
    print dec2
    let msg5 = "fdbeca"
    print "Original message:"
    print msg5
    let enc3 = encode msg5 tree2
    print "Encoded message:"
    print enc3
    let msg6 = [1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0]
    print "Expected encoded message:"
    print msg6
    let dec3 = decode msg6 tree2
    print "Decoded message:"
    print dec3
