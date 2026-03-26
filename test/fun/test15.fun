-- 関数定義
id x = x

-- === 多相性 ===
id x = x

--
const x y = x

--
compose f g x = f (g x)

-- === 再帰関数 ===
fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

--
fact n = if n == 0 then 1 else n * fact (n - 1)

-- === リスト操作 ===
length [] = 0
length (x:xs) = 1 + length xs

--
map f [] = []
map f (x:xs) = f x : map f xs

--
reverse xs = rev xs []
  where
    rev [] acc = acc
    rev (x:xs) acc = rev xs (x:acc)

-- === 型注釈と一致確認 ===
id :: a -> a
id x = x

--
idInt :: Int -> Int
idInt x = x

-- 型注釈と実装が一致しない（型エラー）
badId :: Int -> Bool
badId x = x

-- === 型エラー検出 ===
badAdd = 1 + "hello"         -- Int + String → 型エラー

--
selfApply x = x x            -- 自己適用 → 型エラー

--
addBool x = x + x            -- Bool + Bool → 型エラー

--
testAddBool = addBool True   -- 型エラー

-- === セクション・演算子 ===
f1 = (1 +)       -- Int -> Int

--
f2 = (* 2)       -- Int -> Int

--
f3 = (== 3)      -- Int -> Bool

-- === タプル・レコード ===
pair = (1, True)                         -- (Int, Bool)

--
record = {x = 1, y = True}              -- {x :: Int, y :: Bool}

--
getX r = r.x                            -- record access

-- === condition ===
cond1 = if True then 1 else 2           -- Int

-- === condition ===
cond2 = if True then 1 else "no"        -- type error（Int vs String）

