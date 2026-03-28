hh :: [Int] -> Int
hh [] = 0
hh (xx:xxs) = xx + hh xxs
