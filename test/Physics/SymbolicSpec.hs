{-# LANGUAGE OverloadedStrings #-}

module Physics.SymbolicSpec (spec) where

import Test.Hspec
import SymbolicPhysics.SymbolicD
import qualified Data.Map.Strict as M

spec :: Spec
spec = describe "Physics.Symbolic.Expr" $ do

  describe "constructors" $ do
    it "var creates Var" $
      var "x" `shouldBe` Var "x"

    it "constant creates Const" $
      constant 3.14 `shouldBe` Const 3.14

    it "neg, add, sub, mul, divE, pow, sinE, cosE, tanE, expE, logE" $ do
      neg  (var "x")              `shouldBe` Neg  (Var "x")
      add  (var "x") (constant 2) `shouldBe` Add  (Var "x") (Const 2)
      sub  (constant 5) (var "y") `shouldBe` Sub  (Const 5) (Var "y")
      mul  (var "a") (var "b")     `shouldBe` Mul  (Var "a") (Var "b")
      divE (var "u") (var "v")     `shouldBe` Div  (Var "u") (Var "v")
      pow  (var "p") (constant 2)  `shouldBe` Pow  (Var "p") (Const 2)
      sinE (var "t")               `shouldBe` Sin  (Var "t")
      cosE (var "θ")               `shouldBe` Cos  (Var "θ")
      tanE (var "φ")               `shouldBe` Tan  (Var "φ")
      expE (var "e")               `shouldBe` Exp  (Var "e")
      logE (var "l")               `shouldBe` Log  (Var "l")

  describe "deriv" $ do
    it "d/dx x = 1" $
      deriv "x" (var "x") `shouldBe` Const 1

    it "d/dx y = 0" $
      deriv "x" (var "y") `shouldBe` Const 0

    it "d/dx constant = 0" $
      deriv "x" (constant 42) `shouldBe` Const 0

    it "sum rule: d/dx (x + 3) = 1 + 0" $
      deriv "x" (add (var "x") (constant 3))
        `shouldBe` Add (Const 1) (Const 0)

    it "product rule: d/dx (x * x) = 1*x + x*1" $
      deriv "x" (mul (var "x") (var "x"))
        `shouldBe`
          Add (Mul (Const 1) (Var "x"))
              (Mul (Var "x") (Const 1))

    it "quotient rule: d/dx (x / x) = (1*x - x*1)/(x*x)" $
      deriv "x" (divE (var "x") (var "x"))
        `shouldBe`
          Div
            ( Sub (Mul (Const 1) (Var "x"))
                  (Mul (Var "x") (Const 1)) )
            ( Mul (Var "x") (Var "x") )

    it "chain rule for sin: d/dx sin(x) = cos(x)" $
      deriv "x" (sinE (var "x"))
        `shouldBe` Mul (Const 1) (Cos (Var "x"))

    it "chain rule for cos: d/dx cos(x) = -sin(x)" $
      deriv "x" (cosE (var "x"))
        `shouldBe` Mul (Const 1) (Neg (Sin (Var "x")))

    it "chain rule for tan: d/dx tan(x) = (1 + tan^2(x))" $
      deriv "x" (tanE (var "x"))
        `shouldBe`
          Mul (Const 1)
              (Add (Const 1) (Pow (Tan (Var "x")) (Const 2)))

    it "derivative of exp: d/dx exp(x) = exp(x)" $
      deriv "x" (expE (var "x"))
        `shouldBe` Mul (Const 1) (Exp (Var "x"))

    it "derivative of log: d/dx log(x) = 1/x" $
      deriv "x" (logE (var "x"))
        `shouldBe` Div (Const 1) (Var "x")

  describe "simplify" $ do
    it "folds constants in add" $
      simplify (Add (Const 2) (Const 3)) `shouldBe` Const 5

    it "eliminates zero in add and sub" $ do
      simplify (Add (Const 0) (Var "x")) `shouldBe` Var "x"
      simplify (Sub (Var "y") (Const 0)) `shouldBe` Var "y"

    it "folds constants in mul and div" $ do
      simplify (Mul (Const 4) (Const 2)) `shouldBe` Const 8
      simplify (Div (Const 6) (Const 3)) `shouldBe` Const 2

    it "handles trivial mul/div identities" $ do
      simplify (Mul (Const 1) (Var "x")) `shouldBe` Var "x"
      simplify (Div (Var "x") (Const 1)) `shouldBe` Var "x"

    it "propagates zero in mul" $
      simplify (Mul (Const 0) (Var "x")) `shouldBe` Const 0

  describe "eval" $ do
    let env = M.fromList [("x",2),("y",3)]
    it "evaluates variables and constants" $
      eval env (add (var "x") (constant 5)) `shouldBe` 7
    it "treats missing vars as 0" $
      eval env (var "z") `shouldBe` 0

  describe "pretty" $ do
    it "prints nested expressions with minimal parens" $ do
      pretty (add (var "x") (mul (var "y") (var "z")))
        `shouldBe` "x + y * z"
      pretty (mul (add (var "x") (constant 1)) (var "y"))
        `shouldBe` "(x + 1.0) * y"
