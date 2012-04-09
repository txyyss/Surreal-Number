module Surreal where

import Data.List (subsequences, intercalate, sort, group)

data Surreal = N [Surreal] [Surreal] deriving Show

leq :: Surreal -> Surreal -> Bool
leq x@(N ls _ ) y@(N _ rs) = none (y `leq`) ls && none (`leq` x) rs
  where none = (not . ) . any

valid :: Surreal -> Bool
valid (N ls rs) = (and $ map valid ls) && 
                  (and $ map valid rs) && 
                  (and $ map not [x `leq` y | x <- rs, y <-ls])

instance Eq Surreal where
  x == y = x `leq` y && y `leq` x

instance Ord Surreal where
  (<=) = leq

constructNext :: [Surreal] -> [Surreal]
constructNext x = filter valid [N a b | a <- subsequences x, 
                                b <- subsequences x]

symbolForm :: Surreal -> String
symbolForm (N [] []) = "0"
symbolForm (N [N[] []] []) = "1"
symbolForm (N [] [N[] []]) = "-1"
symbolForm (N ls rs) = "{" ++ listForm ls ++ "|" ++ listForm rs ++ "}"
  where listForm = intercalate "," . map symbolForm

zero = N [] []

one = N [N [] []] []

minusOne = N [] [N [] []]
