module FSA3

where 

import Data.List
import FSA2

apprFact :: (Integer -> Integer) -> Integer -> Integer
apprFact = \ f n -> if n == 0 then 1 else n * f (n-1)

f0:f1:f2:f3:f4:f5:f6:fs = iterate apprFact undefined

fact = fix apprFact

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b 
updates = foldl update 

type Env = String -> Int

data Expr = I Int | V String 
          | Add   Expr Expr 
          | Subtr Expr Expr 
          | Mult  Expr Expr 
          deriving (Eq,Show)

eval :: Expr -> Env -> Int 
eval (I i) c = i 
eval (V name) c = c name
eval (Add   e1 e2) c = (eval e1 c) + (eval e2 c)
eval (Subtr e1 e2) c = (eval e1 c) - (eval e2 c)
eval (Mult  e1 e2) c = (eval e1 c) * (eval e2 c)

assign :: String -> Expr -> Env -> Env 
assign var expr c = let 
  value = eval expr c
 in 
  update c (var,value)

initc :: Env 
initc = undefined

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

example = initc $$ 
          assign "x" (I 3) # 
          assign "x" (Mult (V "x") (V "x")) #
          eval (V "x")

partialSucc = updates undefined [(n,n+1)| n <- [0..100] ]

stronger, weaker :: [a] 
                -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x ==> q x)
weaker   xs p q = stronger xs q p 

test1 = stronger [1..10] (\ x -> even x && x > 3) even
test2 = stronger [1..10] (\ x -> even x || x > 3) even
test3 = stronger [1..10] 
           (\ x -> (even x && x > 3) || even x) even
test4 = stronger [1..10] 
           even (\ x -> (even x && x > 3) || even x) 

pre :: (a -> Bool) -> (a -> b) -> a -> b 
pre p f x = if p x then f x 
            else error "pre"

post :: (b -> Bool) -> (a -> b) -> a -> b 
post p f x = if p (f x) then f x 
             else error "post"

decomp :: Integer -> (Integer,Integer)
decomp n = (0,n) $$ 
           until (odd.snd) (\ (m,k) -> (m+1,div k 2)) 

decompPost :: Integer -> (Integer,Integer)
decompPost = \n -> 
             post (\ (m,k) -> 2^m * k == n) decomp n

assert :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert p f x = if p x (f x) then f x 
               else error "assert"

decompA :: Integer -> (Integer,Integer)
decompA = assert (\ n (m,k) -> 2^m * k == n) decomp

stepA :: (Integer, Integer) ->  (Integer, Integer)
stepA = assert (\ (m,k) (m',k') -> 2^m*k == 2^m'*k')
               (\ (m,k) -> (m+1,div k 2))

invar :: (a -> Bool) -> (a -> a) -> a -> a
invar p f x = 
  let 
    x' = f x 
  in
  if p x && not (p x') then error "invar"
  else x'

succI = invar (>0) succ

predI = invar (<0) pred

largestOddFactor =  while even 
                          (invar (>0) (`div` 2))

predI' = invar (>0) pred

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

sortedProp :: Ord a => [a] -> [a] -> [a] -> Bool
sortedProp xs ys zs = 
  (sorted xs && sorted ys) ==> sorted zs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True 
sorted (x:y:zs) = x <= y && sorted (y:zs)

sublistProp :: Eq a => [a] -> [a] -> [a] -> Bool
sublistProp xs ys zs = sublist xs zs && sublist ys zs

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x:xs) ys = elem x ys && sublist xs (ys\\[x])

assert2 ::  (a -> b -> c -> Bool) 
             -> (a -> b -> c) -> a -> b -> c
assert2 p f x y = 
  if p x y (f x y) then f x y
  else error "assert2"

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y 
                         then x : merge xs (y:ys) 
                         else y : merge (x:xs) ys

mergeA :: Ord a => [a] -> [a] -> [a]
mergeA = assert2 sortedProp 
            $ assert2 sublistProp merge

mergeSrt ::  Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

split :: [a] -> ([a],[a])
split xs = let 
   n = (length xs) `div` 2
  in 
   (take n xs, drop n xs)

euclid = assert2 (\ m n _ -> m > 0 && n > 0) eucl

testEuclid1 :: Int -> Bool 
testEuclid1 k = let 
    primes = take k (filter prime [2..])
  in 
    and [ euclid p q == 1 |  
          p <- primes, q <- primes, p /= q ]

prime :: Integer -> Bool
prime n = 
  n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

forall = flip all

divides :: Integer -> Integer -> Bool
divides n m = rem m n == 0

isGCD :: Integer -> Integer -> Integer -> Bool
isGCD k m n = divides n k && divides n m && 
              forall [1..min k m] 
              (\ x -> (divides x k && divides x m) 
                        ==> divides x n)

euclid' :: Integer -> Integer -> Integer
euclid' = assert2 isGCD euclid

testEuclid :: Integer -> Bool 
testEuclid k = 
  and [ (assert2 isGCD euclid) n m > 0 | 
                       n <- [1..k], m <- [1..k] ]

divisors :: Integer -> Integer -> [Integer]
divisors m n = let 
    k = min m n 
  in [ d | d <- [2..k], divides d m, divides d n ]

sameDivisors (x,y) (x',y') = 
   divisors x y == divisors x' y'

euclidGCD' :: (Integer,Integer) -> (Integer,Integer)
euclidGCD' = while 
             (\ (x,y) -> x /= y) 
             (assert sameDivisors
             (\ (x,y) -> if x > y 
                         then (x-y,y) 
                         else (x,y-x)))

euclidGCD'' :: Integer -> (Integer,Integer) -> Integer
euclidGCD'' = \ d -> whiler 
             (\ (x,y) -> x /= y) 
             (invar (\ (x,y) -> 
                divides d x && divides d y)
             (\ (x,y) -> if x > y 
                         then (x-y,y) 
                         else (x,y-x)))
             fst

testEuclid2 :: Integer -> Bool 
testEuclid2 k = 
  and [ euclidGCD'' d (n,m) >= 0 |  
       n <- [1..k], m <- [1..k], d <- [2..min n m] ]

ext_gcd :: Integer -> Integer -> (Integer,Integer) 
ext_gcd a b = (a,b,0,1,1,0) $$
              whiler 
                 (\ (_,b,_,_,_,_) ->  b /= 0) 
                 (\ (a,b,x,y,lastx,lasty) -> let 
                    (q,r)   = quotRem a b 
                    (x',lastx') = (lastx-q*x,x)
                    (y',lasty') = (lasty-q*y,y)
                 in (b,r,x',y',lastx',lasty'))
                 (\ (_,_,_,_,lx,ly) -> (lx,ly))

bezout :: 
   Integer -> Integer -> (Integer,Integer) -> Bool
bezout m n (x,y) = x*m + y*n == euclid m n 

ext_gcdA = assert2 bezout ext_gcd

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fct_gcd b r 
     in (t, s - q*t)

fct_gcdA = assert2 bezout fct_gcd

