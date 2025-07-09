{-# LANGUAGE OverloadedStrings #-}
module EventRuleSpec (spec) where

import Test.Hspec
import Components        (Component(AtomicC))
import BooleanUtils.Literal           (emptyLiteral, literalFromList, unionLiteral, getLiteral)
import BooleanUtils.Transition        (singletonPhen, epsilon)
import Action            (applyActionWorld, applyActionPhen)
import BooleanUtils.EventRule         (EventRule(..), ruleToAction)
import qualified Data.Set as Set

spec :: Spec
spec = describe "EventRule module" $ do

  let t0 = 0
      a  = AtomicC "a"
      b  = AtomicC "b"
      dom = Set.fromList [a,b]

      swapRule = EventRule
        { erDomain = dom
        , erStep   = \lit ->
            let s = getLiteral lit
            in literalFromList
                 [ if x == a then b
                   else if x == b then a
                   else x
                 | x <- Set.toList s ]
        , erEvents = \t _ -> singletonPhen (epsilon a t)
        }
      act = ruleToAction swapRule

  it "ruleToAction advances the tick by 1" $ do
    let (t1, _) = applyActionWorld [act] t0 emptyLiteral
    t1 `shouldBe` t0 + 1

  it "ruleToAction leaves outside components unchanged" $ do
    let lit0      = unionLiteral (literalFromList [a,b]) emptyLiteral
        (_, lit1) = applyActionWorld [act] t0 lit0
    getLiteral lit1 `shouldSatisfy` Set.member a
    getLiteral lit1 `shouldSatisfy` Set.member b

  it "ruleToAction applies erStep on its domain" $ do
    let lit0      = literalFromList [a]
        (_, lit1) = applyActionWorld [act] t0 lit0
    getLiteral lit1 `shouldBe` Set.fromList [b]

  it "ruleToAction phen yields erEvents phenomena" $ do
    applyActionPhen [act] t0 emptyLiteral
      `shouldBe` singletonPhen (epsilon a t0)
