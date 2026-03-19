-- 無限型（Occurs check）エラー
loop :: a -> a
loop x = x x
