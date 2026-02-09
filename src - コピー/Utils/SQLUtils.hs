module Utils.SQLUtils (extractSQLVars) where

extractSQLVars :: String -> (String, [String])
extractSQLVars = go "" [] ""
  where
    go acc vars current [] =
      (acc, reverse vars)

    go acc vars current ('{':xs) =
      let (var, rest) = span (/= '}') xs
      in go (acc ++ "?") (var : vars) "" (drop 1 rest)

    go acc vars current (x:xs) =
      go (acc ++ [x]) vars current xs
