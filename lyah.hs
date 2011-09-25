{-
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
-}

first :: (a, b, c) -> a
first (x, _, _) = x


second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

{-
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
-}

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

{-
bmiTell ::  Double -> String
bmiTell bmi
 | bmi <= 18.5 = "You're underweight, you emo, you!"
 | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
 | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
 | otherwise = "You're a whale, congratulations!"
-}

bmiTell :: Double -> Double -> String
bmiTell weight height
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

{-
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = firstname
       (l:_) = lastname
-}
initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

{-
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-}

head' :: [a] -> a
head' xs = case xs of [] ->  error "No head for empty lists!"
                      (x:_) -> x
{-
describeList :: [a] -> String                      
describeList ls = "THe list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
-}

describeList :: [a] -> String                                               
describeList ls = "The list is " ++ what ls
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

