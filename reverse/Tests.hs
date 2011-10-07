{-- 
    Author: Ronald Kaiser
    Description: My Reverse testing module.
    Email: <raios dot catodicos @ gmail dot com>

    
    Notes: These tests are not supposed to be exhaustive.
    Just to help in TDD thinking and modeling.

    Running under GHC 7.0.3

    In order to run:
       $ ghc Tests.hs
       $ ./Tests
--}


module Main
where
import Test.HUnit
import Reverse (reverse')


main = do runTestTT tests

tests = TestList [
  reverseOneElementList,
  reverseStringsList,
  reverseNumbersList,
  reverseBools
  ]

reverseOneElementList = 
 reverse' [1] ~?= [1]

reverseStringsList = 
 reverse' "uoy" ~?= 'y':'o':'u':[]

reverseNumbersList =
 reverse' [2, 4] ~?= [4, 2]

reverseBools =
 reverse' [True, False, False] ~?= [False, False, True]
