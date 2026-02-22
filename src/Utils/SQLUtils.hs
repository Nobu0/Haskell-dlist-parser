module Utils.SQLUtils (extractSQLVars) where

-- | Extracts SQL variables from a parameter string and replaces them with placeholders.
--
-- This function processes a string containing variable references in the format @{varName}@
-- and returns a tuple containing:
--
-- * The modified string with all @{varName}@ patterns replaced by @?@ placeholders
-- * A list of extracted variable names in reverse order of appearance
--
-- Examples:
--
-- >>> extractSQLVars "SELECT * FROM users WHERE id = {userId} AND name = {userName}"
-- ("SELECT * FROM users WHERE id = ? AND name = ?",["userName","userId"])
--
-- >>> extractSQLVars "SELECT * FROM products"
-- ("SELECT * FROM products",[])
--
-- >>> extractSQLVars "INSERT INTO logs VALUES ({timestamp}, {message})"
-- ("INSERT INTO logs VALUES (?, ?)",["message","timestamp"])
extractSQLVars :: String -> (String, [String])
extractSQLVars = go "" [] ""
  where
    go acc vars current [] =
      (acc, reverse vars)
    go acc vars current ('{' : xs) =
      let (var, rest) = span (/= '}') xs
       in go (acc ++ "?") (var : vars) "" (drop 1 rest)
    go acc vars current (x : xs) =
      go (acc ++ [x]) vars current xs
