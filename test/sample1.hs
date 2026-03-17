unifyMany :: [Type] -> Either InferError Subst
unifyMany [] = Right emptySubst
unifyMany (t : ts) =
  foldM
    ( ( \sacc t' ->
          case unify (apply sacc t) (apply sacc t') of
            Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t'))
            Right s -> Right (composeSubst s sacc)
      )
        emptySubst
        ts
    )
