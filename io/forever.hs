import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give some input: "
    l <- getLine
    putStrLn $ map toUpper l
