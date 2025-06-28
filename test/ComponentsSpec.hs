{-# LANGUAGE OverloadedStrings #-}
module ComponentsSpec (spec) where

import Test.Hspec
import Components

spec :: Spec
spec = describe "Components module" $ do

  let a = AtomicC "a"
      b = AtomicC "b"
      c = Composite [a,b]
      d = Composite [a, Composite [b,a]]

  it "atomic has no children and arity 0" $ do
    children a `shouldBe` []
    arity a     `shouldBe` 0

  it "composite children and arity match" $ do
    children c `shouldBe` [a,b]
    arity c     `shouldBe` 2

  it "preserves ordering: Composite [a,b] ≠ Composite [b,a]" $ do
    Composite [a,b] `shouldNotBe` Composite [b,a]

  it "parentheses matter: nested composites differ" $ do
    Composite [a,Composite [b,a]] `shouldNotBe`
      Composite [Composite [a,b],a]

  it "subcomponents includes self and all descendants" $ do
    subcomponents d `shouldBe`
      [ d
      , a
      , Composite [b,a]
      , b
      , a
      ]
