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
import Combinatorics (perm, comb)


main = do runTestTT tests

tests = TestList [ permWithOneElement
                 , permWithTwoElements
                 , permWithThreeElements
                 , combWithOneElement
                 , combTakeOneFromTwoElements
                 , combTakeTwoFromTwoElements]

permWithOneElement = 
  perm [1] ~?= [[1]]

permWithTwoElements =
  perm [True,False] ~?= [ [True, False]
                        , [False, True]]

permWithThreeElements = 
  perm ['a', 'b', 'c'] ~?= [ ['a', 'b', 'c']
                           , ['a', 'c', 'b'] 
                           , ['b', 'a', 'c']
                           , ['b', 'c', 'a']
                           , ['c', 'a', 'b']
                           , ['c', 'b', 'a']] 

combWithOneElement =
  comb [1] 1 ~?= [[1]]

combTakeOneFromTwoElements =
  comb [1,2] 1 ~?= [[1], [2]]

combTakeTwoFromTwoElements =
  comb [1, 2] 2 ~?= [[1,2]]

combTakeTwofromThreeElements =
  comb ['a', 'b', 'c'] 2 ~?= [ ['a', 'b']
                             , ['a', 'c']
                             , ['b', 'c']]
                                      
