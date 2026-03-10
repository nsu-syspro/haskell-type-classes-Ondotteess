{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Task2 where

import Task1 (Parse, Parse(..))

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving (Show, Eq)

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving (Show, Eq)

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
--
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse s = case foldl step (Just []) (words s) of
    Just [e] -> Just e
    _ -> Nothing
    where
      step :: Maybe [Expr a op] -> String -> Maybe [Expr a op]
      step Nothing _ = Nothing
      step (Just stack) token =
        case parse token :: Maybe op of
          Just op -> case stack of
            y:x:xs -> Just (BinOp op x y : xs)
            _ -> Nothing
          Nothing -> case parse token :: Maybe a of
            Just a -> Just (Lit a : stack)
            Nothing -> Just (Var token : stack)

instance Parse IntOp where
  parse "+" = Just Add
  parse "*" = Just Mul
  parse "-" = Just Sub
  parse _ = Nothing

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
  evalBinOp Add x y = x + y
  evalBinOp Mul x y = x * y
  evalBinOp Sub x y = x - y

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit x) = Just x
evalExpr m (Var x) = lookup x m
evalExpr m (BinOp op e1 e2) =
  case (evalExpr m e1, evalExpr m e2) of
    (Just x, Just y) -> Just (evalBinOp op x y)
    _ -> Nothing

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate @Integer @IntOp

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'forall a op.' part is required to define generic type
-- of intermediate 'Expr' expression that uses scoped type variables 'a' and 'op'.
--
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing