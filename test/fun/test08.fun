-- 高階関数の適用
applyTwice :: (a -> a) -> a -> a
applyTwice ff xx = ff (ff xx)
