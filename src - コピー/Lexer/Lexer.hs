module Lexer.Lexer
  ( runLexer   -- ← 外部に公開する関数
  ) where

import Lexer.Token
import Lexer.SimpleLexer
import Lexer.LayoutLexer
import Layout.LayoutTransform

runLexer :: String -> [Token]
runLexer = layoutTransform . layoutLexer . slexer
