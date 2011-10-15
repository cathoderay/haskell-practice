{--
    Author: Ronald Kaiser
    Description: This module provides combinatorial functions.
    Email : <raios dot catodicos @ gmail dot com>
    Github: http://github.com/cathoderay/haskell-practice


    PERMUTATIONS:
    =============

    1. First approach:

    Permutations can be dealt recursively.
    We can think of permutations from [a, b, c, d] as:

    [a] ++ perm [b, c, d]
    [b] ++ perm [a, c, d]
    [c] ++ perm [a, b, d]
    [d] ++ perm [a, b, c]

    So, if we have a way of removing one element from a given position, 
    we can calculate permutations taking each element from the original list
    as the first choice concatenated with all the perm from the rest of the list.


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
    comb (x:xs) n = map (x:) (comb xs (n - 1)) ++ comb xs n

--}

module Combinatorics
where


removeNth :: Int -> [a] -> [a]
removeNth n xs = take n xs ++ drop (n + 1) xs

perm :: [a] -> [[a]]
perm [x] = [[x]]
perm xs = concat $ map (\(i, v) -> (map (v:) . perm $ removeNth i xs)) (zip [0..] xs)


comb :: [a] -> Integer -> [[a]]
comb _ 0 = [[]]
comb [] _ = []
comb (x:xs) n = map (x:) (comb xs (n - 1)) ++ comb xs n

