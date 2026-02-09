module AST.Program where


data Program = Program
  { moduleDecl :: Maybe Decl
  , imports    :: [Decl]
  , decls      :: [Decl]
  }
