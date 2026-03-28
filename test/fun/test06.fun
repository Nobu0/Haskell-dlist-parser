-- リストのマップ
map :: (a -> b) -> [a] -> [b]
map ff [] = []
map ff (xx:xxs) = ff xx : map ff xxs
