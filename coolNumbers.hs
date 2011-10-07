{--
  Sum of first numbers with digits adding up to 40.
--}

import Data.Char (digitToInt)
import Data.List (find)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Int -> Maybe Int
firstTo40 n =  find (\x -> digitSum x == n) [1..]

