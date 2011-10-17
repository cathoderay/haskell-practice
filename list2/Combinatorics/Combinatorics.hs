{--
    Author: Ronald Kaiser
    Description: This module provides combinatorial functions.
    Email : <raios dot catodicos @ gmail dot com>
    Github: http://github.com/cathoderay/haskell-practice


    PERMUTATIONS:
    =============

    1. First approach (recursion + removeNth):

    Permutations can be dealt recursively. We can think of permutations from [a, b, c, d] as:

    [a] ++ perm [b, c, d]
    [b] ++ perm [a, c, d]
    [c] ++ perm [a, b, d]
    [d] ++ perm [a, b, c]

    So, if we have a way of removing one element from a given position, 
    we can calculate permutations taking each element from the original list
    as the first choice concatenated with all the permutations from the rest 
    of the list.

    The following code expresses this idea: 
    removeNth :: Int -> [a] -> [a]
    removeNth n xs = take n xs ++ drop (n + 1) xs

    perm :: [a] -> [[a]]
    perm [x] = [[x]]
    perm xs = concat (map (\(i, v) -> (map ([v]++) $ perm (removeNth i xs))) (zip [0..] xs))

    Since ':' is 'cheaper' than '++', we can use 'v:' instead of '[v]++', improving
    permutations to:
    perm :: [a] -> [[a]]
    perm [x] = [[x]]
    perm xs = concat (map (\(i, v) -> (map (v:) $ perm (removeNth i xs))) (zip [0..] xs))

    Using '$' and '.':
    perm :: [a] -> [[a]]
    perm [x] = [[x]]
    perm xs = concat $ map (\(i, v) -> (map (v:) . perm $ removeNth i xs)) (zip [0..] xs)


    2. Second approach (recursion + remove):

    Instead of removing from a given position, we could remove the first element
    we find, with a full search:
    remove :: Eq(a) => [a] -> a -> [a]
    remove [] _ = []
    remove (x:xs) e
      | x == e    = xs
      | otherwise = x:remove xs e

    or perhaps, without guards:
    remove :: Eq(a) => [a] -> a -> [a]
    remove [] _ = []
    remove (x:xs) e = if x == e then xs else x:remove xs e

    Refactoring to use the new remove function: 
    perm :: Eq(a) => [a] -> [[a]]
    perm [] = [[]]
    perm xs = concat (map (\x -> map (x:) (perm (remove xs x))) xs)

    Using '$' and:
    perm :: Eq(a) => [a] -> [[a]]
    perm [] = [[]]
    perm xs = concat $ map (\x -> map (x:) (perm $ remove xs x)) xs


    3. third approach (recursion + list comprehension):

    Making use of 'remove' function defined previously, a list comprehension may
    be used:
    perm Eq(a) => [a] -> [[a]]
    perm [] = [[]]
    perm xs = [x: | x <- xs, ys <- perm (remove xs x)]

   
    COMBINATIONS:
    =============
    
    1. First approach:
    
    Combinations from [a, b, c, d] taking 2 elements can be arranged like:
    [a] ++ comb [b, c, d] 1
    [b] ++ comb [c, d] 1
    [c] ++ comb [d] 1
    
    So, a recursive solution would be:
    comb :: [a] -> Integer -> [[a]]
    comb _ 0 = [[]]
    comb [] _ = []
    comb (x:xs) k = comb xs k ++ map (x:) (comb xs (k - 1))
--}


module Combinatorics
( comb
, perm
) where


remove :: Eq(a) => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) e
  | x == e    = xs
  | otherwise = x:remove xs e


perm :: Eq(a) => [a] -> [[a]]
perm [] = [[]]
perm xs = [x:ys | x <- xs, ys <- perm (remove xs x)]


comb :: [a] -> Int -> [[a]]
comb _ 0 = [[]]
comb [] _ = []
comb (x:xs) k = comb xs k ++ map (x:) (comb xs (k - 1))

