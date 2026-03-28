-- 関数合成
compose :: (b -> c) -> (a -> b) -> a -> c
compose ff gg xx = ff (gg xx)
