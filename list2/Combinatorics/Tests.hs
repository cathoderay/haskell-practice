{-- 
    Author: Ronald Kaiser
    Description: My Combinatorics testing module.
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
import Combinatorics (permutations)


main = do runTestTT tests

tests = TestList [
  permutationsWithOneElement,
  permutationsWithTwoElements,
  permutationsWithThreeElements
  ]

permutationsWithOneElement = 
 permutations [1] ~?= [[1]]

permutationsWithTwoElements =
 permutations [True,False] ~?= [[True, False],
                                [False, True]]

permutationsWithThreeElements = 
 permutations ['a', 'b', 'c'] ~?= [['a', 'b', 'c'],
                                   ['a', 'c', 'b'], 
                                   ['b', 'a', 'c'],
                                   ['b', 'c', 'a'],
                                   ['c', 'a', 'b'],
                                   ['c', 'b', 'a']] 


