{-# LANGUAGE OverloadedStrings #-}
module LiteralSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

import Literal
import Components (Component(AtomicC, Composite))

spec :: Spec
spec = describe "Literal module" $ do

  let a = AtomicC "a"
      b = AtomicC "b"
      c = Composite [a,b]
      lit0 = emptyLiteral
      lit1 = literalFromList [a,b,a]  -- duplicates

  it "emptyLiteral is empty" $ do
    getLiteral lit0 `shouldBe` Set.empty

  it "literalFromList removes duplicates" $ do
    getLiteral lit1 `shouldBe` Set.fromList [a,b]

  it "containsLiteral works" $ do
    containsLiteral a lit1 `shouldBe` True
    containsLiteral c lit1 `shouldBe` False

  it "unionLiteral combines sets" $ do
    let lit2 = literalFromList [c]
    getLiteral (unionLiteral lit1 lit2)
      `shouldBe` Set.fromList [a,b,c]

  it "disjointUnionLiteral succeeds when disjoint" $ do
    disjointUnionLiteral (literalFromList [a]) (literalFromList [b])
      `shouldBe` Just (literalFromList [a,b])

  it "disjointUnionLiteral fails when overlapping" $ do
    disjointUnionLiteral (literalFromList [a,b]) (literalFromList [b,c])
      `shouldBe` Nothing
