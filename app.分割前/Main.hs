module Main where

import Expr (parseExpr)
import Lexer (runLexer)
import Text.Megaparsec.Error (errorBundlePretty)

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
