h :: [Int] -> Int
h [] = 0
h (x:xs) = x + h xs
