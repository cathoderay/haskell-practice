addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


first :: (a, b, c) -> a
first (x, _, _) = x


second :: (a, b, c) -> b
second (_, y, _) = y


third :: (a, b, c) -> c
third (_, _, z) = z


head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x 
tell (x:y:[]) = "The list has two elements : " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are : " ++ show x ++ " and " ++ show y


badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:_) = x + y + z


firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


bmiTell ::  Double -> String
bmiTell bmi
 | bmi <= 18.5 = "You're underweight, you emo, you!"
 | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
 | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
 | otherwise = "You're a whale, congratulations!"


bmiTell' :: Double -> Double -> String
bmiTell' weight height
 | bmi <= skinny  = "You're underweight, you emo, you!"
 | bmi <= normal  = "You're supposedly normal. Pffft, I bet you're ugly!"
 | bmi <= fat     = "You're fat! Lose some weight, fatty!" 
 | otherwise      = "You're a whale, congratulations!"
 where bmi = weight / height ^ 2
       (skinny, normal, fat) = (18.5, 25.0, 30.0)


max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a


myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a == b    = EQ
 | a <= b    = LT
 | otherwise = GT


badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name


initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = firstname
       (l:_) = lastname


initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."


calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2


cylinder :: Double -> Double -> Double
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea


calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


head'' :: [a] -> a
head'' xs = case xs of [] ->  error "No head for empty lists!"
                       (x:_) -> x


describeList :: [a] -> String
describeList ls = "THe list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."


describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
 where what [] = "empty."
       what [x] = "a singleton list."
       what xs = "a longer list."


maximum' :: Ord a => [a] -> a
maximum' [] = error "can't get maximum from empty list."
maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)


replicate' :: Int -> a -> [a]
replicate' n x
 | n <= 0 = []
 | otherwise = x : replicate' (n - 1) x


take' :: (Num i, Ord i) =>  i -> [a] -> [a]
take' n _
 | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


repeat' :: a -> [a]
repeat' x = x:repeat' x


zip' ::  [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)


elem' :: Eq(a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
 | x == y = True
 | otherwise = x `elem'` ys


qsort :: Ord(a) => [a] -> [a]
qsort [] = []
qsort (p:xs) = qsort [x | x <- xs, x < p] ++ [p] ++ qsort [x | x <- xs, x >= p]


--Sectioning infix functions
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a ->  a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)


flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x):(map' f xs)


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
 | f x       = x:filter' f xs
 | otherwise = filter f xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter' (<= x) xs
      larger = filter' (> x) xs
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
 | p x       = x:takeWhile' p xs
 | otherwise = []


--Collatz conjecture
chain :: Integer -> [Integer]
chain 1 = [1]
chain n 
 | odd n     = n:chain (3 * n + 1)
 | otherwise = n:chain (div n 2)

--Finding the number of collatz sequences with length > 15 
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15


--Mapping with multiple parameters! =)
--ghci> let listOfFuns = map (*) [0..]
--ghci>(listOfFuns !! 4) 5
--20


--Refactoring numLongChains with lambda
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))


--foldl use; very similar with reduce from clojure
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


--refactoring, taking into account that functions can be curried, =)
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0


map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs 


map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs


elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys 


maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max


reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> acc ++ [x]) []

--more efficient than the previous one
reverse''' :: [a] -> [a]
reverse''' = foldl (\acc x -> x:acc) []

--using flip
reverse'''' :: [a] -> [a]
reverse'''' = foldl (flip (:)) []


product' :: Num(a) => [a] -> a
product' = foldl (*) 1


filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []


last' :: [a] -> a
last' = foldl1 (\_ x -> x)


and' :: [Bool] -> Bool
and' = foldr (&&) True


(&&*) :: Bool -> Bool -> Bool
True &&* x = x
False &&* _ = False


sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1


--function application
($*) :: (a -> b) -> a -> b
f $* x = f x


--function composition
(.*) :: (b -> c) -> (a -> b) -> a -> c
f .* g = \x -> f (g x)


--point-free style
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]


findKey :: (Eq k) =>  k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs)
  | key == k  = Just v
  | otherwise = findKey key xs

--Usually better, reading a recursion takes more time than fold
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs


data Shape = Circle Float Float Float | Rectangle Float Float Float Float
 deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
