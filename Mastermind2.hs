module Mastermind

where 

import Data.List

data Colour   = Red | Yellow | Blue | Green  | Orange
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

type Pattern  = [Colour]
type Feedback = [Answer]

samepos :: Pattern -> Pattern -> Int
samepos _      []                 = 0 
samepos []     _                  = 0 
samepos (x:xs) (y:ys) | x == y    = samepos xs ys + 1
                      | otherwise = samepos xs ys 

occurscount ::  Pattern -> Pattern -> Int
occurscount xs []       = 0
occurscount xs (y:ys) 
          | y `elem` xs = occurscount 
                          (delete y xs) ys + 1
          | otherwise   = occurscount xs ys 

reaction :: Pattern -> Pattern -> [Answer]
reaction secret guess = take n (repeat Black) 
                     ++ take m (repeat White)
    where n = samepos secret guess 
          m = occurscount secret guess - n


makeList :: [a] -> Int -> [[a]]
makeList xs 1 = [[x] | x <- xs]
makeList xs n = [[x] ++ y | x <- xs, y <- makeList xs $ n-1]

firstList = makeList [Red,Yellow,Blue,Green,Orange] 4

guessing :: Pattern -> Pattern -> [Pattern] -> [Pattern]
guessing secret guess xs = filter (\x -> reaction x guess == reaction secret guess) xs


exercise1play :: Pattern -> [Pattern] -> Int -> Int
exercise1play secret (x:[]) n = if x == secret then n else -1
exercise1play secret (x:xs) n = exercise1play secret (guessing secret x (x:xs)) n+1

exercise1 :: Pattern -> Int
exercise1 secret = exercise1play secret firstList 0


exercise2min :: [(Pattern, Int)] -> Pattern
exercise2min xs = fst $ (filter (\ (_,b) -> b == minimum (map snd xs)) xs) !! 0

exercise2max :: [(Pattern, [[Feedback]])] -> [(Pattern, Int)]
exercise2max xs = map (\ (a,b) -> (a,maximum b)) $ map (\ (a,b) -> (a,map length b)) xs

exercise2list :: [Pattern] -> [(Pattern, [[Feedback]])]
exercise2list xs = map (\ (a,b) -> (a, group b)) $ [(maybeGuess, [reaction maybeSecret maybeGuess | maybeSecret <- xs]) | maybeGuess <- xs]

exercise2play :: Pattern -> [Pattern] -> Int -> Int
exercise2play secret (x:[]) n = if (x == secret) then n else -1
exercise2play secret xs n = exercise2play secret (guessing secret (exercise2min $ exercise2max $ exercise2list xs) xs) n+1

exercise2 :: Pattern -> Int
exercise2 secret = exercise2play secret firstList 0


exercise3max :: [(Pattern, Int)] -> Pattern
exercise3max xs = fst $ (filter (\ (_,b) -> b == maximum (map snd xs)) xs) !! 0

exercise3prep :: [(Pattern, [[Feedback]])] -> [(Pattern, Int)]
exercise3prep xs = map (\ (a,b) -> (a,length b)) xs

exercise3play :: Pattern -> [Pattern] -> Int -> Int
exercise3play secret (x:[]) n = if (x == secret) then n else -1
exercise3play secret xs n = exercise3play secret (guessing secret (exercise3max $ exercise3prep $ exercise2list xs) xs) n+1

exercise3 :: Pattern -> Int
exercise3 secret = exercise3play secret firstList 0


exercise4sum :: [(Pattern, [[Feedback]])] -> [(Pattern, Int)]
exercise4sum xs = map (\ (a,b) -> (a,sum $ map (^2) b)) $ map (\ (a,b) -> (a,map length b)) xs

exercise4play :: Pattern -> [Pattern] -> Int -> Int
exercise4play secret (x:[]) n = if (x == secret) then n else -1
exercise4play secret xs n = exercise4play secret (guessing secret (exercise2min $ exercise4sum $ exercise2list xs) xs) n+1

exercise4 :: Pattern -> Int
exercise4 secret = exercise4play secret firstList 0


exercise5min :: [(Pattern, Float)] -> Pattern
exercise5min xs = fst $ (filter (\ (_,b) -> b == minimum (map snd xs)) xs) !! 0

exercise5entropy :: [(Pattern, [[Feedback]])] -> [(Pattern, Float)]
exercise5entropy xs = map (\ (a,b) -> (a,sum $ map (\ x -> fromIntegral x * (logBase (fromIntegral $ length b) $ fromIntegral x)) $ b)) $ map (\ (a,b) -> (a,map length b)) xs

exercise5play :: Pattern -> [Pattern] -> Int -> Int
exercise5play secret (x:[]) n = if (x == secret) then n else -1
exercise5play secret xs n = exercise5play secret (guessing secret (exercise5min $ exercise5entropy $ exercise2list xs) xs) n+1

exercise5 :: Pattern -> Int
exercise5 secret = exercise5play secret firstList 0
