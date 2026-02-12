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

```bash
./run_all.sh
