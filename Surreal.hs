module Surreal where

data Surreal = N [Surreal] [Surreal] deriving Show

-- Definition of less than or equal to

leq :: Surreal -> Surreal -> Bool
leq x@(N ls _ ) y@(N _ rs) = none (y `leq`) ls && none (`leq` x) rs
  where none = (not . ) . any
        
valid :: Surreal -> Bool
valid (N ls rs) = and $ map not [x `leq` y | x <- rs, y <-ls]
