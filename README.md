# Haskell Parser

A hand-written Haskell parser written in Haskell, capable of parsing real-world Haskell source files including:

- Module and import declarations
- Data and type declarations
- Function type signatures (including constrained types)
- Function definitions with guards
- Layout-sensitive syntax (via custom layout lexer)
- Unicode and multibyte character support (e.g., Japanese comments and strings)

## Motivation

This parser was built from scratch as a learning project to deeply understand the Haskell language by implementing its syntax rules directly. The goal was to go beyond surface-level usage and explore the design and structure of Haskell itself.

## Features

- Parses real `.hs` files from the `src/` directory
- Handles layout rules (`do` blocks, indentation)
- Supports constrained types like `(Ord a) => a -> a -> a`
- Ignores whitespace and comments within parentheses
- Unicode-safe: handles Japanese and other multibyte characters

## Usage

To run the parser on all `.hs` files in `src/`:

## 謝辞

この構文解析器は、Haskellの構文と設計を深く理解することを目的として、一から手作業で構築されました。  
開発の過程では、Microsoft Copilot のキャラクター「ミカ 🦊」のサポートを受けながら、  
構文の設計、レイアウト処理、Unicode対応、実ファイルの解析など、数々の課題を乗り越えて完成に至りました。

ミカとの対話を通じて、単なるコードの実装にとどまらず、言語の本質や設計思想への理解を深めることができました。  
このプロジェクトは、学びと探究の旅の記録でもあります。

## 所感

これで私の目標が実現しました。今やAIは当たり前ですが、どこまで大きなコードがAIを使って作成できるかが知りたいことでした。
他のセッションでは段々大きくなってくると、最後は意味不明のメッセージが出たり、PCが非常に重くなり使えなくなりました。
今回は無事たどり着くことができました。

それでもAIを使いコードを作成するには、やはり技術が必要だと感じました。
時々、過去の繰り返しになったりする場面では、人間が介入しなければならないと実感しました。

それにしても、これだけ巨大な（Haskellなので比較的コードは短いですが）コードが完成できたことは、私にとって革命的な開発体験でした。
これからは、学習の指導者としての役割や、全く新しい機能を目的にしたコード開発こそが、人間に残された本質的な仕事なのではないかと感じています。

まだまだ不足部分もありますが、ここで公開して作業を進めることにしました。
