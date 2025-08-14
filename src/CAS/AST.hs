module CAS.AST ( Expr(..)
  , (.+.), (.-.), (.*.), (./.), (.^.)
  , neg
  ) where

data Expr
  = Var String
  | Const Rational
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Neg Expr
  | Sin Expr
  | Cos Expr
  | Tan Expr
  | Exp Expr
  | Log Expr
  deriving (Eq, Ord, Show)

infixl 6 .+., .-.
infixl 7 .*., ./.
infixr 8 .^.

(.+.) :: Expr -> Expr -> Expr
(.+.) = Add

(.-.) :: Expr -> Expr -> Expr
(.-.) = Sub

(.*.) :: Expr -> Expr -> Expr
(.*.) = Mul

(./.) :: Expr -> Expr -> Expr
(./.) = Div

(.^.) :: Expr -> Expr -> Expr
(.^.) = Pow

neg :: Expr -> Expr
neg = Neg
