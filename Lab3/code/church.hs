{-# OPTIONS_GHC -fwarn-tabs #-}

module Church where

-- Church encoding of zero
-- Just the identity function.
zero :: (a -> a) -> a -> a
zero _ x = x

-- Church encoding of successor
-- Create a function that applies f one additional time.
successor :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a)
successor n = \f x -> (f (n f x))

-- Addition of Church encoded numerals
-- Apply f on x m times, then apply f on the result n times (m+n)
add :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> ((a -> a) -> a -> a)
add n m = \f x -> (n f (m f x))

-- Multiplication of Church encoded numerals
-- Create a new lambda that applies f m times, and apply this lambda n times (m*n)
mult :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> ((a -> a) -> a -> a)
mult n m = \f x -> (n (\x -> (m f x)) x)

-- Define some numbers
one = (successor zero)
two = (successor one)
three = (successor two)
four = (successor three)
five = (successor four)

-- General purpose increment function, for tests
inc :: Integer -> Integer
inc n = n + 1

-- Some test numbers:
-- Run tests with (function inc 0)
-- E.g. (ten inc 0) applies the inc function to 0 10 times, yielding the number 10.
-- May swap the function, base to apply a different function n times to a certain base.
six = (add two four)
seven = (add three four)
eight = (add four four)
nine = (add five four)
ten = (add five five)
eleven = (add six five)
twelve = (mult three four)
thirteen = (add three (mult five two))
fourteen = (mult seven two)
fifteen = (mult (add three two) three)
twentyfive = (mult five five)
