-- ERROR 無限型（Occurs check）エラー
loop :: a -> a
loop xx = xx xx
