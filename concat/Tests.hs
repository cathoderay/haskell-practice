{-- 
    Author: Ronald Kaiser
    Description: My Concat testing module.
    Email: <raios dot catodicos @ gmail dot com>

    
    Notes: These tests are not supposed to be exhaustive.
    Just to help in TDD thinking and modeling.

    Running under GHC 7.0.3
--}


module Main
where
import Test.HUnit
import Concat (concat')


main = do runTestTT tests

tests = TestList [
  concatJustANullString,
  concatJustAString,
  concatWithStrings,
  concatStringWithNullString,
  concatNullStrings,
  concatWithNumbers
  ]

concatJustANullString = 
 concat' [""] ~?= ""

concatJustAString =  
 concat' ["a"] ~?= 'a':[]

concatWithStrings =  
 concat' ["fo", "o"] ~?= 'f':'o':'o':[]

concatStringWithNullString =  
 concat' ["haskel", "", "l"] ~?= "haskell"

concatNullStrings =  
 concat' ["", ""] ~?= ""

concatWithNumbers =  
 concat' [[3], [1, 4], [], [1, 5, 9]] ~?= [3, 1, 4, 1, 5, 9]

