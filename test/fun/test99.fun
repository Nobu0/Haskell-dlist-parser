extractSQLVars :: String -> (String, [String])
extractSQLVars = go "" []
  where
    go acc vars [] = (acc, reverse vars)
    go acc vars ('{' : xs) =
      let (var, rest) = span (/= '}') xs
      in go (acc ++ "?") (var : vars) (drop 1 rest)
    go acc vars (x : xs) =
      go (acc ++ [x]) (vars ++ []) xs
