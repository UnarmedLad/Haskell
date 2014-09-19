module FSA2

where

import Data.List

fibon n = fibon' n 0 1 0 

fibon' n x y k = if k == n then x 
                 else fibon' n y (x+y) (k+1)

fix :: (a -> a) -> a
fix f = f (fix f)

fib :: Integer -> Integer
fib n = fib2 0 1 n

fib2 :: Integer -> Integer -> Integer -> Integer
fib2 = fix (\ f x y n -> 
            if n == 0 then x
            else f y (x+y) (n-1))

run :: Integer -> [Integer]
run n = run1 [n]

run1 ::  [Integer] -> [Integer]
run1 = fix (\ f ns -> 
          let 
            n = head ns 
          in 
            if n == 1 then ns
            else if even n then f (div n 2:ns)
            else f (3*n+1:ns))

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f = \ x -> 
  if p x then while p f (f x)
         else x 

g = while even (`div` 2)

lfp ::  Eq a => (a -> a) -> a -> a
lfp f = until (\x -> x == f x) f 

lfp' :: Eq a => (a -> a) -> a -> a
lfp' f = while (\x -> x /= f x) f 

lf :: Eq a => (a -> a) -> a -> a
lf f = fix 
        (\ g x -> if x == f x then x else g (f x))

whiler :: (a -> Bool) -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = r . while p f

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

fb :: Int -> Int
fb n = (0,1,0) $$
       whiler (\ (_,_,k) -> k<n) 
              (\ (x,y,k) -> (y,x+y,k+1)) 
              (\ (x,_,_) -> x) 

eucl m n = (m,n) $$
           whiler 
              (\ (x,y) -> x /= y) 
              (\ (x,y) -> if x > y then (x-y,x) 
                                   else (x,y-x)) 
              fst

repeat :: (a -> a) -> (a -> Bool) -> a -> a
repeat f p = while (not.p) f . f

repeatr :: (a -> a) -> (a -> Bool) -> (a -> b) -> a -> b
repeatr f p r = whiler (not.p) f r . f 

for :: [a] -> (a -> b -> b) -> b -> b
for [] f y = y
for (x:xs) f y = for xs f (f x y)

fact :: Integer -> Integer
fact n = 1 $$ for [1..n] (*)

factorial :: Integer -> Integer
factorial m = (1,m) $$ 
              whiler (\ (_,n) ->  n /= 0) 
                     (\ (t,n) -> (n*t,n-1))
                     fst

forr :: [a] -> (a -> b -> b) -> (b -> c) -> b -> c
forr xs f r = r . for xs f 

fordown :: [a] -> (a -> b -> b) -> b -> b
fordown = for . reverse

forrdown :: [a] -> (a -> b -> b) -> (b -> c) -> b -> c
forrdown = forr . reverse

