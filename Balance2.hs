module Balance

where 

import Data.List
import Debug.Trace
myShow xs = traceShow xs xs 

data Coin = Light | Normal deriving (Eq,Show,Bounded,Enum)
data Feedback = Leftbound | Balanced | Rightbound deriving (Eq,Show)
data ScalePos = L | R | Off deriving (Eq,Show)

type Pattern = [Coin]
type Weighing = [ScalePos]

count :: [Coin] -> Int
count [] = 0
count (x:xs) = if x == Light then 1 + count xs else if x == Normal then 2 + count xs else error "Undefined coin"

gatherSide :: Pattern -> Weighing -> ScalePos -> [Coin]
gatherSide [] [] _ = []
gatherSide (x:xs) (y:ys) side = if y == side then (x:gatherSide xs ys side) else gatherSide xs ys side

reaction :: Pattern -> Weighing -> Feedback
reaction secret guess = if left > right then Leftbound else if left < right then Rightbound else Balanced where {left = count (gatherSide secret guess L); right = count (gatherSide secret guess R)}

makeList :: [a] -> Int -> [[a]]
makeList xs 1 = [[x] | x <- xs]
makeList xs n = [[x] ++ y | x <- xs, y <- makeList xs $ n-1]

guessing :: Pattern -> Weighing -> [Pattern] -> [Pattern]
guessing secret guess xs = filter (\x -> reaction x guess == reaction secret guess) xs


exercise6amin :: [(Weighing, Int)] -> Weighing
exercise6amin xs = fst $ (filter (\ (_,b) -> b == minimum (map snd xs)) xs) !! 0

exercise6amax :: [(Weighing, [[Feedback]])] -> [(Weighing, Int)]
exercise6amax xs = map (\ (a,b) -> (a,maximum b)) $ map (\ (a,b) -> (a,map length b)) xs

exercise6alist :: Int -> [Pattern] -> [(Weighing, [[Feedback]])]
exercise6alist n xs = map (\ (a,b) -> (a, group b)) $ [(maybeGuess, [reaction maybeSecret maybeGuess | maybeSecret <- xs]) | maybeGuess <- makeList [L,R,Off] n]

exercise6aplay :: Int -> Pattern -> [Pattern] -> Int -> Int
exercise6aplay _ secret (x:[]) i = if x == secret then i else -1
exercise6aplay n secret xs i = exercise6aplay n secret (guessing secret (myShow $ exercise6amin $ exercise6amax $ exercise6alist n xs) xs) (i+1)

exercise6a :: Int -> Pattern -> Int
exercise6a n secret = exercise6aplay n secret firstList 0 where firstList = makeList [Light,Normal] n


exercise6bmin :: [(Weighing, Float)] -> Weighing
exercise6bmin xs = fst $ (filter (\ (_,b) -> b == minimum (map snd xs)) xs) !! 0

exercise6bentropy :: [(Weighing, [[Feedback]])] -> [(Weighing, Float)]
exercise6bentropy xs = map (\ (a,b) -> (a,sum $ map (\ x -> fromIntegral x * (log $ fromIntegral x)) $ b)) $ map (\ (a,b) -> (a,map length b)) xs

exercise6bplay :: Int -> Pattern -> [Pattern] -> Int -> Int
exercise6bplay _ secret (x:[]) i = if x == secret then i else -1
exercise6bplay n secret xs i = exercise6bplay n secret (guessing secret (myShow $ exercise6bmin $ exercise6bentropy $ exercise6alist n xs) xs) (i+1)

exercise6b :: Int -> Pattern -> Int
exercise6b n secret = exercise6bplay n secret firstList 0 where firstList = makeList [Light,Normal] n




