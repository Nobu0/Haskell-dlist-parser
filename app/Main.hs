module Main where

import Control.Monad (forM_)
import Data.Char (ord)
import Debug.Trace (trace, traceShow)
import Expr.AST (Expr)
import Expr.Combinator (Parser (..), runParser)
import Expr.ExprParser (exprTop)
import Expr.Parser (parseExpr, runExprTest, runToplevelTest, toplevel)
import Lexer (Token, runLexer)
import qualified Lexer
import MyTrace (setTrace)
import System.IO (hFlush, stdout)
import Text.Megaparsec.Error (errorBundlePretty)

testCasesDo :: [(String, String)]
testCasesDo =
  [ ( "Simple let-return",
      "do { let x = 1; return x }"
    ),
    ( "Multiple lets",
      "do { let x = 1; let y = 2; return (x + y) }"
    ),
    ( "If in do",
      "do { let x = 10; if x > 5 then return x else return 0 }"
    ),
    ( "Nested do",
      "do { let x = 1; do { let y = x + 1; return y } }"
    ),
    ( "Do in if",
      "if cond then do { let x = 1; return x } else return 0"
    ),
    ( "Let with lambda",
      "do { let f = \\x -> x + 1 * 2; return (f 10) }"
    ),
    ( "Let without semicolon error case",
      "do { let x = 1 return x }" -- 失敗するかも？
    ),
    ( "Do in case",
      "case v of Just x -> do { let y = x + 1; return y }; Nothing -> return 0"
    ),
    ( "Empty do",
      "do { }"
    ),
    ( "Case with as-pattern",
      "case v of x@(Just y) -> return x _ -> return 0"
    ),
    ( "x; y; z statements",
      "x; y; z"
    ),
    ( "Let with case",
      "do { let x = case v of Just n -> n; Nothing -> 0; return x }"
    ),
    ("simple", "{ x = 1 }"),
    ("2", "{ x = 1, y = 2 }"),
    ("tag", "r { x = 3 }"),
    ("nest", "{ a = { b = 3 } }"),
    ("2 equation", "{ x = 1 + 2, y = f 3 }"),
    ("space", "{  x=1 , y =2}"),
    ("", "r { x = 3, y = 4 }"),
    ("", "(f x) { y = 10 }"),
    ("", "{ x = 1, y = 2 } { x = 9 }"),
    ("", "(+ 1)"),
    ("", "(1 +)"),
    ("", "(* 2) (2 *) (> 3) (3 >)"),
    ("", "((+ 1))"),
    ("", "{ f = (+ 1) }"),
    ("", "r { f = (1 +) }"),
    ("", "({ x = 1 } +)"),
    ("", "do { let r = { x = 1 }; return r }"),
    ("", "do { let r = { x = 1 }; return (r { x = 2 }) }"),
    ("", "do { let f = (+ 1); return (f 10) }"),
    ("error1", "{ = 1 }"),
    ("error2", "{ x = 1,, y = 2 }"),
    ("error3", "()"),
    ("error4 Ok", "(1 + 2)"),
    ("case guard1", "case x of { p | cond1 -> e1; p2 | cond2 -> e2; p3 -> e3 }"),
    ("case guard2", "case x of n | n < 0 -> 1 "),
    ("case guard3", "case x of n | n < 0 -> -1 | n > 0 -> 1"),
    ("case guard4", "case x of n | n < 0 -> -1\n  0 -> 0\n  n | n > 0 -> 1"),
    ("list1", "[1, 2, 3]"),
    ("list2", "[x * 2 | x <- xs]"),
    ("list3", "[x | x <- xs, x > 0]"),
    ("list4", "[(x, y) | x <- xs, y <- ys]"),
    ("list5", "[x | let y = f x, y > 0]"),
    ("list6", "[x | Just x <- xs]"),
    ("list7", "do { let xs = [x | x <- ys]; return xs }"),
    ("list8", "[x | let y = f x, y > 0]"),
    ("for1", "for x in xs -> x * 2"),
    ("for2", "for x in xs, y in ys -> (x, y)"),
    ("for3", "for x in xs, y in ys, z in zs -> (x,y,z)"),
    ("for3", "for x in xs, y in ys, z in zs -> ..."),
    ("for4", "for x in xs, x > 0 -> x"),
    ("for5", "for x in xs, let y = f x -> y"),
    ("do1", "do {\n  (x, y) <- f z;\n  return x }"),
    ("do2", "do {\n      let y = f x;\n      z <- g y;\n      print z }"),
    ("do3", "do {\n      x <- xs;\n      if x > 0 then return x else fail }  "),
    {-}
                ("",""),
    -}
    ("do2", "do {\n  let xs = [1,2,3];\n  for x in xs, x > 1 -> x\n}"),
    ("do3", "do {\n  let xs = [1,2,3];\n  for x in xs, let y = f x -> y\n}"),
    ("do4", "do {\n  let xs = [1,2,3];\n  for x in xs -> ...\n}"),
    ("nested do", "do {\n  let x = 1;\n  do {\n    let y = x + 1;\n    return y\n  }\n}"),
    ("complex", "do {\n  let xs = [1,2,3];\n  for x in xs, let y = f x, y > 1 -> y\n}"),
    ("complex2", "do {\n  let xs = [1,2,3];\n  for x in xs, let y = f x, y > 1 -> ...\n}"),
    ("list1", "[1 .. 10]\n    [x .. y]\n    [0 .. -5]\n    [a .. b]"),
    ("list2", "[1, 3 .. 10]\n[10, 8 .. 0]\n[x, y .. z]\n[0, 0 .. 0]"),
    ("list3", "[1,2,3]\n[x, y, z]\n[1, 2, 3,]\n[]\n[   ]"),
    ("list4", "[1, 2 .. 3]     -- rangeStep\n[1 ,2 ..3]      -- whitespace variations\n[1 .. 2, 3]     -- should be normal list? or error?（仕様確認"),
    ("list error1", "[1 ..]          -- エラーになるべき"),
    ("list error2", "[.. 10]         -- エラーになるべき")
  ]

runParserWithTrace :: Parser a -> [Token] -> IO (Maybe a)
runParserWithTrace p tokens = do
  setTrace False -- 最初はトレースOFF
  -- setTrace True
  case runParser p tokens of
    Just (result, []) -> return (Just result) -- 成功 → そのまま返す
    _ -> do
      putStrLn "XX Parser failed! Re-running with trace:"
      setTrace True
      case runParser p tokens of
        Just (result, []) -> return (Just result)
        _ -> return Nothing

main :: IO ()
main = do
  putStrLn "=== Running Parser Test Suite ==="
  forM_ testCasesDo $ \(label, input) -> do
    putStrLn $ "\n-- " ++ label ++ " --\n-- Input: " ++ input
    -- putStrLn $ "Bytes: " ++ show (map ord input)
    case runLexer input of
      Left err -> putStrLn $ "X Lexer error: " ++ show err
      Right toks -> do
        putStrLn $ "Tokens: " ++ show toks
        result <- runParserWithTrace exprTop toks
        print result

{-}
main :: IO ()
main = do
  putStrLn "=== Running Parser Test Suite ==="
  forM_ testCasesDo $ \(label, input) -> do
    putStrLn $ "\n-- " ++ label ++ " --\n-- Input: " ++ input
    case runLexer input of
      Left err -> putStrLn $ "X Lexer error: " ++ show err
      Right toks -> do
        putStrLn $ "Tokens: " ++ show toks
        case runParser exprTop toks of
          Just (result, []) -> do
            putStrLn "Ok Parse succeeded:"
            print result
          Just (result, rest) -> do
            putStrLn "<!>  Partial parse:"
            print result
            putStrLn $ "Unconsumed tokens: " ++ show rest
          Nothing -> putStrLn "XX Parser failed!"
    hFlush stdout
-}

{-}
main :: IO ()
main = do
  -- let input = "k = case xs of { [] -> 0; x:xs -> x + sum xs } where { xs = [1,2,3] }"
  -- let input = "do { x <- f; y <- g; return (x + y) }"
  -- let input = "do { let x = 1; return x }"
  -- let input = "do { if cond then return 1 else return 2 }"
  -- let input = "do { let x = 10; if x > 5 then return x else return 0 }"
  -- let input = "do { }"
  -- let input = "case v of Just x -> do { let y = x + 1; return y }; Nothing -> return 0"
  let input = "do { let x = case v of Just n -> n; Nothing -> 0; return x }"

  case runLexer input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right toks -> do
      putStrLn $ "Tokens: " ++ show toks
      -- case runParser toplevel toks of
      case runParser expr toks of
        Just (result, []) -> traceShow result $ print result
        Just (_, rest) -> traceShow rest $ putStrLn $ "Unconsumed tokens: " ++ show rest
        Nothing -> trace "Parser failed!" $ putStrLn "Parser failed!"
-}

{-}
      case runParser toplevel toks of
        Just (result, []) -> print result
        Just (_, rest) -> putStrLn $ "Unconsumed tokens: " ++ show rest
        Nothing -> putStrLn "Parser failed!"
      hFlush stdout

main :: IO ()
main = do
  -- 入力
  let input = "k = case xs of { [] -> 0; x:xs -> x + sum xs } where { xs = [1,2,3] }"
  -- トップレベルでパース
  case runLexer input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right toks -> do
      putStrLn $ "Tokens: " ++ show toks
      case Expr.Combinator.runParser toplevel toks of
        -- case Expr.Combinator.runParser expr toks of
        -- case runParser expr toks of -- toplevel toks of
        Just (result, []) -> print result
        Just (_, rest) -> putStrLn $ "Unconsumed tokens: " ++ show rest
        Nothing -> putStrLn "Parser failed!"

  let input = "case xs of { [] -> 0; x:xs -> x + sum xs } where { xs = [1,2,3] }"
  -- let input = "1 + 2"
  -- let input = "case x of { _ -> \\y -> y + 1 }"
  case Lexer.runLexer input of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right toks -> do
      putStrLn $ "Tokens: " ++ show toks
      case Expr.Combinator.runParser expr toks of
        Just (result, []) -> print result
        Just (_, rest) -> putStrLn $ "Unconsumed tokens: " ++ show rest
        Nothing -> putStrLn "Parser failed!"
-}

testCases :: [(String, String)]
testCases =
  [ ("x", "EVar \"x\""),
    ("42", "EInt 42"),
    ("1 + 2", "EBinOp \"+\" (EInt 1) (EInt 2)"),
    ("1 + 2 * 3", "EBinOp \"+\" (EInt 1) (EBinOp \"*\" (EInt 2) (EInt 3))"),
    ("let x = 1 in x", "ELet \"x\" (EInt 1) (EVar \"x\")"),
    ("let x = 1 in x + 2", "ELet \"x\" (EInt 1) (EBinOp \"+\" (EVar \"x\") (EInt 2))"),
    ("if x then 1 else 0", "EIf (EVar \"x\") (EInt 1) (EInt 0)"),
    ("if 1 then 2 + 3 else 4", "EIf (EInt 1) (EBinOp \"+\" (EInt 2) (EInt 3)) (EInt 4)"),
    ("\\x -> x", "ELam \"x\" (EVar \"x\")"),
    ("(\\x -> x + 1) 5", "EApp (ELam \"x\" (EBinOp \"+\" (EVar \"x\") (EInt 1))) (EInt 5)"),
    ("(\\x -> \\y -> x + y) 1 2", "EApp (EApp (ELam \"x\" (ELam \"y\" (EBinOp \"+\" (EVar \"x\") (EVar \"y\")))) (EInt 1)) (EInt 2)"),
    ("\\x -> x", "ELam \"x\" (EVar \"x\")"),
    ("\\x -> x + 1", "ELam \"x\" (EBinOp \"+\" (EVar \"x\") (EInt 1))"),
    ("\\x -> \\y -> x + y", "ELam \"x\" (ELam \"y\" (EBinOp \"+\" (EVar \"x\") (EVar \"y\")))"),
    ("(\\x -> x + 1) 5", "EApp (ELam \"x\" (EBinOp \"+\" (EVar \"x\") (EInt 1))) (EInt 5)"),
    ("(\\x -> \\y -> x + y) 1 2", "EApp (EApp (ELam \"x\" (ELam \"y\" (EBinOp \"+\" (EVar \"x\") (EVar \"y\")))) (EInt 1)) (EInt 2)"),
    ("let add = \\x -> \\y -> x + y in add 3 4", "ELet \"add\" (ELam \"x\" (ELam \"y\" (EBinOp \"+\" (EVar \"x\") (EVar \"y\")))) (EApp (EApp (EVar \"add\") (EInt 3)) (EInt 4))"),
    ( "case x of 0 -> 1; 1 -> 2; _ -> 3",
      "ECase (EVar \"x\") [(PInt 0,EInt 1),(PInt 1,EInt 2),(PWildcard,EInt 3)]"
    ),
    ( "case y of n -> n + 1",
      "ECase (EVar \"y\") [(PVar \"n\",EBinOp \"+\" (EVar \"n\") (EInt 1))]"
    ),
    ( "case z of _ -> 42",
      "ECase (EVar \"z\") [(PWildcard,EInt 42)]"
    ),
    ( "case x of 0 -> (case y of 1 -> 10; _ -> 20); _ -> 99",
      "ECase (EVar \"x\") [(PInt 0,ECase (EVar \"y\") [(PInt 1,EInt 10),(PWildcard,EInt 20)]),(PWildcard,EInt 99)]"
    ),
    ( "case x of _ -> \\y -> y + 1",
      "ECase (EVar \"x\") [(PWildcard,ELam \"y\" (EBinOp \"+\" (EVar \"y\") (EInt 1)))]"
    ),
    ( "let f = \\x -> case x of 0 -> 1; _ -> x in f 5",
      "ELet \"f\" (ELam \"x\" (ECase (EVar \"x\") [(PInt 0,EInt 1),(PWildcard,EVar \"x\")])) (EApp (EVar \"f\") (EInt 5))"
    ),
    ( "let x = let y = 2 in y + 3 in x * 4",
      "ELet \"x\" (ELet \"y\" (EInt 2) (EBinOp \"+\" (EVar \"y\") (EInt 3))) (EBinOp \"*\" (EVar \"x\") (EInt 4))"
    ),
    ( "if x then if y then 1 else 2 else 3",
      "EIf (EVar \"x\") (EIf (EVar \"y\") (EInt 1) (EInt 2)) (EInt 3)"
    ),
    ("f x y", "EApp (EApp (EVar \"f\") (EVar \"x\")) (EVar \"y\")"),
    ("(f x) y", "EApp (EApp (EVar \"f\") (EVar \"x\")) (EVar \"y\")"),
    ("f (x y)", "EApp (EVar \"f\") (EApp (EVar \"x\") (EVar \"y\"))"),
    ("(x + y) * z", "EBinOp \"*\" (EBinOp \"+\" (EVar \"x\") (EVar \"y\")) (EVar \"z\")"),
    ("x + y * z", "EBinOp \"+\" (EVar \"x\") (EBinOp \"*\" (EVar \"y\") (EVar \"z\"))"),
    ("x * y + z", "EBinOp \"+\" (EBinOp \"*\" (EVar \"x\") (EVar \"y\")) (EVar \"z\")"),
    ( "case x of Just y -> y; Nothing -> 0",
      "ECase (EVar \"x\") [(PConstr \"Just\" [PVar \"y\"],EVar \"y\"),(PConstr \"Nothing\" [],EInt 0)]"
    ),
    ( "case x of (a, b) -> a + b",
      "ECase (EVar \"x\") [(PTuple [PVar \"a\",PVar \"b\"],EBinOp \"+\" (EVar \"a\") (EVar \"b\"))]"
    ),
    ( "let id = \\x -> x in id (id 5)",
      "ELet \"id\" (ELam \"x\" (EVar \"x\")) (EApp (EVar \"id\") (EApp (EVar \"id\") (EInt 5)))"
    ),
    ( "(\\x -> x x) (\\x -> x)",
      "EApp (ELam \"x\" (EApp (EVar \"x\") (EVar \"x\"))) (ELam \"x\" (EVar \"x\"))"
    ),
    ("(1, 2)", "ETuple [EInt 1, EInt 2]"),
    ("(1, 2, 3)", "ETuple [EInt 1, EInt 2, EInt 3]"),
    ("(x, y + 1)", "ETuple [EVar \"x\", EBinOp \"+\" (EVar \"y\") (EInt 1)]"),
    ("let t = (1, 2) in fst t", "ELet \"t\" (ETuple [EInt 1, EInt 2]) (EApp (EVar \"fst\") (EVar \"t\"))"),
    ("[1, 2, 3]", "EList [EInt 1, EInt 2, EInt 3]"),
    ("[]", "EList []"),
    ("[x, y + 1, 3]", "EList [EVar \"x\", EBinOp \"+\" (EVar \"y\") (EInt 1), EInt 3]"),
    ("let xs = [1,2] in head xs", "ELet \"xs\" (EList [EInt 1, EInt 2]) (EApp (EVar \"head\") (EVar \"xs\"))"),
    ("case xs of [] -> 0; x:xs -> x", "ECase (EVar \"xs\") [(PList [],EInt 0),(PCons (PVar \"x\") (PVar \"xs\"),EVar \"x\")]"),
    ("case p of (x, y) -> x + y", "ECase (EVar \"p\") [(PTuple [PVar \"x\",PVar \"y\"],EBinOp \"+\" (EVar \"x\") (EVar \"y\"))]"),
    ( "case x of { Just y -> y; Nothing -> 0 }",
      "ECase (EVar \"x\") [(PConstr \"Just\" [PVar \"y\"],EVar \"y\"),(PConstr \"Nothing\" [],EInt 0)]"
    ),
    ( "case xs of { [] -> 0; x:xs -> x }",
      "ECase (EVar \"xs\") [(PList [],EInt 0),(PCons (PVar \"x\") (PVar \"xs\"),EVar \"x\")]"
    ),
    ( "case p of { (x, y) -> x + y }",
      "ECase (EVar \"p\") [(PTuple [PVar \"x\",PVar \"y\"],EBinOp \"+\" (EVar \"x\") (EVar \"y\"))]"
    ),
    ( "case x of { 0 -> (case y of { 1 -> 10; _ -> 20 }); _ -> 99 }",
      "ECase (EVar \"x\") [(PInt 0,ECase (EVar \"y\") [(PInt 1,EInt 10),(PWildcard,EInt 20)]),(PWildcard,EInt 99)]"
    ),
    ( "case x of { _ -> \\y -> y + 1 }",
      "ECase (EVar \"x\") [(PWildcard,ELam \"y\" (EBinOp \"+\" (EVar \"y\") (EInt 1)))]"
    ),
    -- 単純な内包表記
    ( "[x | x <- [1,2,3]]",
      "EListComp (EVar \"x\") [EGenerator \"x\" (EList [EInt 1,EInt 2,EInt 3])]"
    ),
    -- 複数の生成子
    ( "[ (x, y) | x <- [1,2], y <- [3,4] ]",
      "EListComp (ETuple [EVar \"x\",EVar \"y\"]) [EGenerator \"x\" (EList [EInt 1,EInt 2]),EGenerator \"y\" (EList [EInt 3,EInt 4])]"
    ),
    -- 生成子＋ガード
    ( "[ x | x <- [1..10], even x ]",
      "EListComp (EVar \"x\") [EGenerator \"x\" (ERange (EInt 1) (EInt 10)),EGuard (EApp (EVar \"even\") (EVar \"x\"))]"
    ),
    -- ガードだけ（生成子なし）
    ( "[ x | even x ]",
      "EListComp (EVar \"x\") [EGuard (EApp (EVar \"even\") (EVar \"x\"))]"
    ),
    -- 空の内包表記（構文的には許容されるなら）
    ( "[ x | ]",
      "EListComp (EVar \"x\") []"
    ),
    ( "let x = 1; y = 2 in x + y",
      "ELet [(\"x\",EInt 1),(\"y\",EInt 2)] (EBinOp \"+\" (EVar \"x\") (EVar \"y\"))"
    ),
    ( "let a = 10 in a",
      "ELet [(\"a\",EInt 10)] (EVar \"a\")"
    ),
    ( "(x + 1) :: Int",
      "EAnn (EBinOp \"+\" (EVar \"x\") (EInt 1)) (TCon \"Int\")"
    ),
    ( "xs :: [Int]",
      "EAnn (EVar \"xs\") (TList (TCon \"Int\"))"
    ),
    ( "f :: a -> b",
      "EAnn (EVar \"f\") (TArrow (TVar \"a\") (TVar \"b\"))"
    ),
    ( "f :: Eq a => a -> Bool",
      "EAnn (EVar \"f\") (TConstraint [Constraint \"Eq\" [TVar \"a\"]] (TArrow (TVar \"a\") (TCon \"Bool\")))"
    ),
    ( "f :: (Eq a, Show a) => a -> String",
      "EAnn (EVar \"f\") (TConstraint [Constraint \"Eq\" [TVar \"a\"], Constraint \"Show\" [TVar \"a\"]] (TArrow (TVar \"a\") (TCon \"String\")))"
    ),
    ( "f :: Ord a => [a] -> a",
      "EAnn (EVar \"f\") (TConstraint [Constraint \"Ord\" [TVar \"a\"]] (TArrow (TList (TVar \"a\")) (TVar \"a\")))"
    ),
    ( "f :: Maybe a",
      "EAnn (EVar \"f\") (TApp (TCon \"Maybe\") (TVar \"a\"))"
    ),
    ( "f :: Either a b",
      "EAnn (EVar \"f\") (TApp (TApp (TCon \"Either\") (TVar \"a\")) (TVar \"b\"))"
    ),
    ( "f :: State s a -> s",
      "EAnn (EVar \"f\") (TArrow (TApp (TApp (TCon \"State\") (TVar \"s\")) (TVar \"a\")) (TVar \"s\"))"
    ),
    ( "f :: Maybe (a -> b)",
      "EAnn (EVar \"f\") (TApp (TCon \"Maybe\") (TArrow (TVar \"a\") (TVar \"b\")))"
    ),
    ( "f :: Either (Maybe a) [b]",
      "EAnn (EVar \"f\") (TApp (TApp (TCon \"Either\") (TApp (TCon \"Maybe\") (TVar \"a\"))) (TList (TVar \"b\")))"
    ),
    ( "f :: (a -> b) -> Maybe c",
      "EAnn (EVar \"f\") (TArrow (TArrow (TVar \"a\") (TVar \"b\")) (TApp (TCon \"Maybe\") (TVar \"c\")))"
    ),
    ( "f :: (a -> b) -> (c -> d)",
      "EAnn (EVar \"f\") (TArrow (TArrow (TVar \"a\") (TVar \"b\")) (TArrow (TVar \"c\") (TVar \"d\")))"
    ),
    ( "f :: (Eq (Maybe a), Show [b]) => Either (Maybe a) [b] -> String",
      "EAnn (EVar \"f\") (TConstraint [Constraint \"Eq\" [TApp (TCon \"Maybe\") (TVar \"a\"]), Constraint \"Show\" [TList (TVar \"b\")]] (TArrow (TApp (TApp (TCon \"Either\") (TApp (TCon \"Maybe\") (TVar \"a\"))) (TList (TVar \"b\"))) (TCon \"String\")))"
    ),
    ( "f :: forall a b. a -> b",
      "EAnn (EVar \"f\") (TForall [\"a\",\"b\"] (TArrow (TVar \"a\") (TVar \"b\")))"
    )
  ]

testCases2 :: [(String, String)]
testCases2 =
  [ ( "f x = x + y where { y = 10 }",
      "ELam \"x\" (ELet [(PVar \"y\",EInt 10)] (EBinOp \"+\" (EVar \"x\") (EVar \"y\")))"
    ),
    ( "g = z where { z = 42 }",
      "ELet [(PVar \"z\",EInt 42)] (EVar \"z\")"
    ),
    ( "h = a + b where { a = 1; b = 2 }",
      "ELet [(PVar \"a\",EInt 1),(PVar \"b\",EInt 2)] (EBinOp \"+\" (EVar \"a\") (EVar \"b\"))"
    ),
    ( "k = case xs of { [] -> 0; x:xs -> x + sum xs } where { xs = [1,2,3] }",
      "ELet [(PVar \"xs\",elist [EInt 1,EInt 2,EInt 3])] (ECase (EVar \"xs\") [(PNil,EInt 0),(PCons (PVar \"x\") (PVar \"xs\"),EApp (EApp (EVar \"+\") (EVar \"x\")) (EApp (EVar \"sum\") (EVar \"xs\")))])"
    ),
    ( "id' x = x where { x = 999 }",
      "ELam \"x\" (ELet [(PVar \"x\",EInt 999)] (EVar \"x\"))"
    )
  ]

exprTests :: [(String, String)]
exprTests =
  [ ("1 + 2", "EApp (EApp (EVar \"+\") (EInt 1)) (EInt 2)"),
    ("x + y where { y = 10 }", "ELet [(PVar \"y\",EInt 10)] (EApp (EApp (EVar \"+\") (EVar \"x\")) (EVar \"y\"))")
  ]

toplevelTests :: [(String, String)]
toplevelTests =
  [ ("f x = x + y where { y = 10 }", "ELet [(PVar \"y\",EInt 10)] (EApp (EApp (EVar \"+\") (EVar \"x\")) (EVar \"y\"))"),
    ("g = 42", "EInt 42")
  ]

{-}
  putStrLn "=== Expr Tests ==="
  -- mapM_ runExprTest exprTests
  mapM_ runExprTest testCases

  putStrLn "\n=== Toplevel Tests ==="
  -- mapM_ runToplevelTest toplevelTests
  mapM_ runToplevelTest testCases2
-}
{-}
repl :: IO ()
repl = do
  putStrLn "Enter an expression:"
  input <- getLine
  case runLexer input of
    Left lexErr -> do
      putStrLn "Lexer error:"
      putStrLn (errorBundlePretty lexErr)
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show tokens
      result <- parseExpr tokens
      case result of
        Nothing -> putStrLn "Parser error!"
        Just ast -> do
          putStrLn "Parsed AST:"
          print ast

-- NG: 文字列で比較
-- show parsedExpr == show expectedExpr

-- OK: 構造で比較
-- parsedExpr == expectedExpr

runTests :: IO ()
runTests = do
  putStrLn "Running tests..."
  mapM_ runTest testCases
  where
    runTest (input, expected) = do
      putStrLn $ "Input: " ++ input
      case runLexer input of
        Left err -> putStrLn $ "  Lexer error: " ++ show err
        Right tokens -> do
          putStrLn $ " Tokens: " ++ show tokens
          result <- parseExpr tokens
          case result of
            Nothing -> putStrLn "  Parser error!!!!!!!"
            Just ast ->
              let normalize = filter (not . (`elem` [' ', '\n', '\t']))
                  actual = normalize (show ast)
                  expected' = normalize expected
               in if actual == expected'
                    then putStrLn "  O Passed"
                    else do
                      putStrLn "  X Failed!!!!!!!!!"
                      putStrLn $ "     Expected: " ++ expected
                      putStrLn $ "     Got:      " ++ show ast

main :: IO ()
main = do
  putStrLn "1. Run REPL"
  putStrLn "2. Run tests"
  putStr "Choose: "
  choice <- getLine
  case choice of
    "1" -> repl
    "2" -> runTests
    _ -> putStrLn "Invalid choice"
-}
