-- module Test.DeclParserSpec where
module Main where

import AST.Decl
import AST.Type
import Decl.DeclParserCore (program)
import Lexer.Lexer (runLexer)
import Parser.Core.Combinator (runParser)

main :: IO ()
main = do
  test
    "type Yen = Int"
    (Just [DeclTypeAlias "Yen" [] (TCon "Int")])

  test
    "type Pair a = (a, a)"
    (Just [DeclTypeAlias "Pair" ["a"] (TTuple [TVar "a", TVar "a"])])

  test
    "class Eq a where {}"
    (Just [DeclClass "Eq" ["a"] []])

  test
    "class Show a where { show :: a -> String }"
    ( Just
        [ DeclClass
            "Show"
            ["a"]
            [DeclTypeSig "show" (TArrow (TVar "a") (TCon "String"))]
        ]
    )

  test
    "instance Show Int where {}"
    (Just [DeclInstance Nothing "Show" [TCon "Int"] []])

  test
    "instance (Eq a, Show a) => Ord a where {}"
    ( Just
        [ DeclInstance
            (Just [Constraint "Eq" [TVar "a"], Constraint "Show" [TVar "a"]])
            "Ord"
            [TVar "a"]
            []
        ]
    )

  test
    "instance Eq Int where { (==) :: Int -> Int -> Bool }"
    ( Just
        [ DeclInstance
            Nothing
            "Eq"
            [TCon "Int"]
            [DeclTypeSig "==" (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool")))]
        ]
    )

  test
    "instance (Eq a, Show (Maybe a)) => Ord a where {}"
    ( Just
        [ DeclInstance
            ( Just
                [ Constraint "Eq" [TVar "a"],
                  Constraint "Show" [TApp (TCon "Maybe") (TVar "a")]
                ]
            )
            "Ord"
            [TVar "a"]
            []
        ]
    )
  test
    "max' :: (Ord a) => a -> a -> a"
    ( Just
        [ DeclTypeSig
            "max'"
            ( TConstraint
                [Constraint "Ord" [TVar "a"]]
                (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))
            )
        ]
    )

-- ヘルパー関数：パースして期待値と比較
test :: String -> Maybe [Decl] -> IO ()
test input expected = do
  putStrLn $ "\n-- Test input: " ++ input
  let tokens = runLexer input
  putStrLn $ "Tokens: " ++ show tokens
  let result = case runParser program tokens of
        Just (ast, _) -> Just ast
        Nothing -> Nothing
  putStrLn $ "AST: " ++ show result
  if result == expected
    then putStrLn "L Test passed"
    else do
      putStrLn "X Test failed!"
      putStrLn $ "Expected: " ++ show expected
