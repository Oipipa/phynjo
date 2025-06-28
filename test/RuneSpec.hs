{-# LANGUAGE OverloadedStrings #-}
module RuneSpec (spec) where

import Test.Hspec
import Components      (Component(AtomicC))
import Literal         (emptyLiteral, literalFromList, unionLiteral, getLiteral)
import Transition      (singletonPhen, epsilon)
import Action          (applyActionWorld, applyActionPhen)
import Rune            (Rune(..), toAction)
import qualified Data.Set as Set

spec :: Spec
spec = describe "Rune module" $ do

  let t0 = 0
      a  = AtomicC "a"
      b  = AtomicC "b"
      dom = Set.fromList [a,b]

      swapRune = Rune
        { domainRune = dom
        , fRune      = \lit ->
            let s = getLiteral lit
            in literalFromList [ if x == a then b else if x == b then a else x
                               | x <- Set.toList s ]
        , gRune      = \t _ -> singletonPhen (epsilon a t)
        }
      act = toAction swapRune

  it "toAction advances the tick by 1" $ do
    let (t1, _) = applyActionWorld [act] t0 emptyLiteral
    t1 `shouldBe` t0 + 1

  it "toAction leaves outside components unchanged" $ do
    let lit0      = unionLiteral (literalFromList [a,b]) emptyLiteral
        (_, lit1) = applyActionWorld [act] t0 lit0
    getLiteral lit1 `shouldSatisfy` Set.member a
    getLiteral lit1 `shouldSatisfy` Set.member b

  it "toAction applies fRune on its domain" $ do
    let lit0      = literalFromList [a]
        (_, lit1) = applyActionWorld [act] t0 lit0
    getLiteral lit1 `shouldBe` Set.fromList [b]

  it "toAction phen yields gRune phenomena" $ do
    applyActionPhen [act] t0 emptyLiteral
      `shouldBe` singletonPhen (epsilon a t0)
