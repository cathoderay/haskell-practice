{--
    Author: Ronald Kaiser
        Description: This module provides a reverse function.
    Email : <raios dot catodicos @ gmail dot com>

    Considerations:

    Recursive approach:
    reverse' :: [a] -> [a]
    reverse' [] = []
    reverse' (x:xs) = reverse' xs ++ [x]

    Using foldr:
    reverse' :: [a] -> [a]
    reverse' = foldr (\x acc -> acc ++ [x]) []

    Using foldl:
    reverse' :: [a] -> [a]
    reverse' xs = foldl (\acc x -> x:acc) [] xs
--}

module Reverse
where

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

