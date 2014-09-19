module FSAlab2

where 

import Data.List

data Colour   = Red | Yellow | Blue | Green | Orange 
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

colours = [Red,Yellow,Blue,Green,Orange]

getCombinations :: [a] -> [[a]]
getCombinations na = do a <- na
                        b <- na
                        c <- na
                        d <- na
                        [[a,b,c,d]]
    
origlist = getCombinations colours

purgeOne :: [Pattern] -> Pattern -> [Pattern]
purgeOne (x:xs) secret = filter (\y -> (reaction y x) == (reaction secret x)) (x:xs)

guessOne :: [Pattern] -> Pattern -> Int -> Int
guessOne (x:[]) secret n = if (x == secret) then n else -1
guessOne xs secret n = guessOne (purgeOne xs secret) secret n+1

playOne :: Pattern -> Int
playOne secret = guessOne origlist secret 0

purgeTwo :: [Pattern] -> Pattern -> Pattern -> [Pattern]
purgeTwo xs secret guess = filter (\x -> (reaction secret guess) == (reaction secret x)) xs

listTwo :: [Pattern] -> [[Pattern]]
listTwo xs = [purgeTwo xs i j | i <- xs, j <- xs]

guessTwo :: [Pattern] -> Pattern -> Int -> Int
guessTwo (x:[]) secret n = if (x == secret) then n else -1
guessTwo xs secret n = guessTwo (filter (\x1 -> x1 == purgeTwo xs secret ((filter (\x2 -> (length x2) == minimum (map length (listTwo xs))) (listTwo xs)) !! 0)) xs) secret n+1

playTwo :: Pattern -> Int
playTwo secret = guessTwo origlist secret 0






