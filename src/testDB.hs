import qualified Data.Map as M
import Language.TypeSystem.TypeDB
import Language.TypeSystem.ParseType

main :: IO ()
main = do
  typeDB <- loadTypeDB "types_cleaned.tsv"
  putStrLn "Loaded typeDB:"
  --mapM_ print (M.toList typeDB)

  -- ここで型パーサを作ったら:
  let typeEnv = M.map parseType typeDB
  mapM_ print (M.toList typeEnv)
  -- inferDecls typeEnv decls

  return ()
