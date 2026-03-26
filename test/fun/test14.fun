-- === 基本的な型推論 ===
-- 定数
42

--
"hello"

--
'a'

--
[]

--
()

-- 変数
x

-- 関数適用
f x
f 1

-- Simple let-return
do { let x = 1; return x }

-- Multiple lets
do { let x = 1; let y = 2; return (x + y) }

-- If in do
do { let x = 10; if x > 5 then return x else return 0 }

-- Nested do
do { let x = 1; do { let y = x + 1; return y } }

-- Do in if
if cond then do { let x = 1; return x } else return 0

-- Let with lambda
do { let f = \\x -> x + 1 * 2; return (f 10) }

-- Let without semicolon error case (AST test)
do { let x = 1; return x }

-- Do in case
case v of Just x -> do { let y = x + 1; return y }; Nothing -> return 0

-- Empty do
-- do { }

-- Case with as-pattern
case v of
  x@(Just y) -> return y
  _ -> return 0

-- x; y; z statements
a;b;c

-- Let with case
do { let x = case v of Just n -> n; Nothing -> 0; return x }

-- simple
{ x = 1 }

--
{ x = 1, y = -2 }

-- tag
r { x = 3 }

--
r2 { x = 3, y = 4 }

-- nest
{ a = { b = 3 } }

-- 2 equation
{ x = 1 + 2, y = f 3 }

-- error
-- (f x) { y = 10 }
-- error
-- { x = 1, y = 2 } { x = 9 }

-- etc
(+ 1)

--
(1 +)

-- OK: 関数合成
((> 3) . (* 2))

--
((+ 1))

--
{ f = (+ 1) }

--
r3 { f = (1 +) }

-- error
-- ({ x = 1 } +)

-- do1
do { let r = { x = 1 }; return r }

-- do2
do { let r = { x = 1 }; return (r { x = 2 }) }

-- do3
do { let f = (+ 1); return (f 10) }

-- case guard1
case x of { p | cond1 -> 1; p2 | cond2 -> 2; p3 -> 3 }

-- case guard2
case x of n | n < 0 -> 1 

-- case guard3
case x of n | n < 0 -> -1 | n > 0 -> 1

-- case guard4
case x of n | n < 0 -> -1;  0 -> 0;  n | n > 0 -> 1

--list1
[1, 2, 3]

--list2
[x * 2 | x <- xs]

--list3
[x | x <- xs, x > 0]

--list4
[(x, y) | x <- xs, y <- ys]

--list5
[x | let y = f x, y]

--list6
[x | Just x <- xs]

--list7
do { let xs = [x | x <- ys]; return xs }

--for1
for x in xs -> x * 2

--for2
for x in xs, y in ys -> (x, y)

--for3
for x in xs, y in ys, z in zs -> (x,y,z)

--for3
for x in xs, y in ys, z in zs -> x + y + z

--for4
for x in xs, x > 0 -> x

--for5
[ y | x <- xs, let y = f x ]
--do{ let xs = [1,2]; for x in xs, let y = f x -> y}

--do1
do {  (x, y) <- ft z;  return x }

--do2
do { let y = f x; z <- g y; return z }

--do3
--do { x <- xs; if x > 0 then return x else fail }

--do2
do {  let xs = [1,2];  for x in xs, x > 1 -> x}

--nested do
do {  let x = 1;  do {    let y = x + 1;    return y  }}

--complex
-- do {  let xs = [1,2];  for x in xs, let y = f x, y > 1 -> y}

--list1
[1 .. 10]
[0 .. -5]

--list2
[1, 3 .. 10]
[10, 8 .. 0]
[0, 0 .. 0]

--list3
[1,2,3]
[1, 2, 3,]
[]
[   ]

--list4
[1, 5 ..]
[1, 2 .. 3]
--[1 .. 2, 3]

--func2
case x of Just n | n > 0 -> n   | n < 1 -> 2  where n = 10

-- func3
do { x <- foo;  y <- bar;  x + y} where foo = 1; bar = 2

--func4
[x | Just x <- xs] where xs = [1..10]

--func5
let  fi x = x + 1;  g z = fi z * 2 in g 10

--func6
x + y where  ff x = x + 1;  y = ff 10

--func7
let f x = x + 1 in 
  case f 3 of 
    n | n > 3 -> n
      | otherwise -> 0
  where otherwise = True

--sql1
sql "SELECT * FROM users"

--sql2
sql "SELECT * FROM users WHERE id = {x}"

--sql3
sql "INSERT INTO t(a,b) VALUES({a}, {b})"

--sql4
sql "UPDATE t SET a={a}, b={b}, c={c}"

--sqlx1
let x = sql "SELECT * FROM t" in x

--sqlx2
-- f (sql "SELECT * FROM t WHERE id = {a}")

--sqlx3
x where y = sql "SELECT * FROM t WHERE a = {a}"

--sqlx4
do { x <- sql "SELECT * FROM t";  return x }

--sqlx6
let x = sql "SELECT {a}" in x where y = sql "SELECT {b}"

--let1
let (a, b) = (1, 2) in a + b

--let1
do {let (a, b) = (1, 2) in let f = a + b in f}

--list1
let x = [] in (1 : x)

--$1
-- print $ a b c

--$2
do { print "test" }

--$3
do { print "test"; return 0 }

