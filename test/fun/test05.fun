-- リストの長さ
length :: [a] -> Int
length [] = 0
length (_:xxs) = 1 + length xxs
