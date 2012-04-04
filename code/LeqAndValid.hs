leq :: Surreal -> Surreal -> Bool
leq x@(N ls _ ) y@(N _ rs) = none (y `leq`) ls && none (`leq` x) rs
  where none = (not . ) . any
valid :: Surreal -> Bool
valid (N ls rs) = (and $ map valid ls) && 
                  (and $ map valid rs) && 
                  (and $ map not [x `leq` y | x <- rs, y <-ls])
