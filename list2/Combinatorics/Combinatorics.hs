{--
    Author: Ronald Kaiser
    Description: This module provides combinatorial functions.
    Email : <raios dot catodicos @ gmail dot com>
    Github: http://github.com/cathoderay/haskell-practice


    PERMUTATIONS:
    ================

    1. First approach:

    Permutations can be dealt recursively.
    We can think of permutations from [a, b, c, d] as:

    [a] ++ permutations [b, c, d]
    [b] ++ permutations [a, c, d]
    [c] ++ permutations [a, b, d]
    [d] ++ permutations [a, b, c]

    So, if we have a way of removing one element from a given position, 
    we can calculate permutations taking each element from the original list
    as the first choice concatenated with all the permutations from the rest of the list.


    The following code expresses this idea: 

    removeNth :: Int -> [a] -> [a]
    removeNth n xs = take n xs ++ drop (n + 1) xs

    permutations :: [a] -> [[a]]
    permutations [x] = [[x]]
    permutations xs = concat (map (\(i, v) -> (map ([v]++) $ permutations (removeNth i xs))) (zip [0..] xs))


    Since ':' is 'cheaper' than '++', we can use 'v:' instead of '[v]++', improving
    permutations to:
    permutations :: [a] -> [[a]]
    permutations [x] = [[x]]
    permutations xs = concat (map (\(i, v) -> (map ([v]++) $ permutations (removeNth i xs))) (zip [0..] xs))


    Using '$' and '.':
    permutations :: [a] -> [[a]]
    permutations [x] = [[x]]
    permutations xs = concat $ map (\(i, v) -> (map (v:) . permutations $ removeNth i xs)) (zip [0..] xs)


--}

module Combinatorics
where


removeNth :: Int -> [a] -> [a]
removeNth n xs = take n xs ++ drop (n + 1) xs

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = concat $ map (\(i, v) -> (map (v:) . permutations $ removeNth i xs)) (zip [0..] xs)

