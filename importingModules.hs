import Data.List (nub)
import qualified Data.Map as M


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


