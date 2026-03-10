{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Task1 (Parse(..))
import Task2 (Expr(..), Eval(..), evalExpr)

data BoolOp = And | Or | Xor
  deriving Show

instance Parse BoolOp where
  parse "and" = Just And
  parse "or" = Just Or
  parse "xor" = Just Xor
  parse _ = Nothing

instance Eval Bool BoolOp where
  evalBinOp And x y = x && y
  evalBinOp Or x y = x || y
  evalBinOp Xor x y = x /= y

vars :: Expr Bool BoolOp -> [String]
vars (Lit _) = []
vars (Var x) = [x]
vars (BinOp _ l r) = vars l ++ vars r

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
  | x `elem` xs = unique xs
  | otherwise = x : unique xs

assignments :: [String] -> [[(String, Bool)]]
assignments [] = [[]]
assignments (v:vs) = [(v, b) : rest | b <- [False, True], rest <- assignments vs]

parseBoolExpr :: String -> Maybe (Expr Bool BoolOp)
parseBoolExpr s = case foldl step (Just []) (words s) of
  Just [e] -> Just e
  _ -> Nothing
  where
    step :: Maybe [Expr Bool BoolOp] -> String -> Maybe [Expr Bool BoolOp]
    step Nothing _ = Nothing
    step (Just (y:x:xs)) token =
      case parse token of
        Just op -> Just (BinOp op x y : xs)
        Nothing -> Just (Var token : y : x : xs)
    step (Just xs) token =
      case parse token of
        Just (_ :: BoolOp) -> Nothing
        Nothing -> Just (Var token : xs)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT s = case parseBoolExpr s of
  Nothing -> Nothing
  Just expr ->
    let vs = unique (vars expr)
        as = assignments vs
    in Just (any (\m -> evalExpr m expr == Just True) as)