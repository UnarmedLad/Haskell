mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs
