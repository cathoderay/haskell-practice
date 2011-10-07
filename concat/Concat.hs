{--
    Author: Ronald Kaiser
    Description: This module provides a concatenation function.
    Email : <raios dot catodicos @ gmail dot com>


    Considerations:
    If we have a non-empty lists, we could use foldl1:
    concat' :: [[a]] -> [a]
    concat' = foldl1 (++)


    or foldr1, in the same way:
    concat' :: [[a]] -> [a]
    concat' = foldr1 (++)


    For the more general case, we could use a recursive approach:
    concat' :: [[a]] -> [a]
    concat' [] = []
    concat' (x:xs) = x ++ concat' xs

    
    a foldl:
    concat' :: [[a]] -> [a]
    concat' = foldl (\acc x -> acc ++ x) [] xs


    or foldr:
    concat' :: [[a]] -> [a]
    concat' = foldr (\x acc -> x ++ acc) []


    Using currying:
    concat' :: [[a]] -> [a]
    concat' xs = foldr (++) [] xs
--}

module Concat
where

concat' :: [[a]] -> [a]
concat' = foldr (++) []  

