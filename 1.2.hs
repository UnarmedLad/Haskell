length = foldr (\ _ n -> n + 1) 0
elem x = foldr (\ y b -> (x == y) || b) False
or = foldr (||) True
map f = foldr (\ x xs -> (f x):xs) []
filter p = foldr (\ x xs -> if p x then x:xs else xs) []
(++) = foldr (\ x xs -> xs Main.++ [x])
