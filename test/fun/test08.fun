-- 高階関数の適用
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
