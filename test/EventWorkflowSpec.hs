{-# LANGUAGE OverloadedStrings #-}
module EventWorkflowSpec (spec) where

import Test.Hspec
import qualified Data.Set        as Set

import Literal               (emptyLiteral, literalFromList)
import Transition            (singletonPhen, epsilon)
import EventRule             (EventRule(..), ruleToAction)
import EventWorkflow         (EventWorkflow(..), toProcess
                             , applyEventWorkflowWorld
                             , applyEventWorkflowPhen)
import Components            (Component(AtomicC))
import Action                (applyActionWorld, applyActionPhen)

spec :: Spec
spec = describe "EventWorkflow module" $ do

  let t0 = 0
      lit0 = emptyLiteral
      a = AtomicC "a"

      -- no‐op rule: empty domain, identity step, no events
      noOpRule = EventRule
        { erDomain = Set.empty
        , erStep   = id
        , erEvents = \_ _ -> Set.empty
        }

      -- phen‐only rule on 'a'
      phenRule = EventRule
        { erDomain = Set.fromList [a]
        , erStep   = id
        , erEvents = \t _ -> singletonPhen (epsilon a t)
        }

      wNoOp = ERun noOpRule
      wPhen = ERun phenRule

  it "ERun noOp advances tick and leaves flags" $ do
    applyEventWorkflowWorld wNoOp t0 lit0 `shouldBe` (t0+1, lit0)
    applyEventWorkflowPhen  wNoOp t0 lit0 `shouldBe` Set.empty

  it "ERun phenRule emits an event but leaves state" $ do
    let (t1, l1) = applyEventWorkflowWorld wPhen t0 lit0
    t1 `shouldBe` t0+1
    l1 `shouldBe` lit0
    applyEventWorkflowPhen wPhen t0 lit0
      `shouldBe` singletonPhen (epsilon a t0)

  it "ESeq composes two runs in time" $ do
    let w2 = ESeq wNoOp wPhen
        (t2, _) = applyEventWorkflowWorld w2 t0 lit0
    t2 `shouldBe` t0+2
    applyEventWorkflowPhen w2 t0 lit0
      `shouldBe` singletonPhen (epsilon a (t0+1))

  it "EPar runs two workflows in parallel" $ do
    let wPar = EPar wPhen wPhen
        (tPar, _) = applyEventWorkflowWorld wPar t0 lit0
    tPar `shouldBe` t0+1
    applyEventWorkflowPhen wPar t0 lit0
      `shouldBe`
        Set.union
          (singletonPhen (epsilon a t0))
          (singletonPhen (epsilon a t0))

  it "Nested ESeq/EPar works" $ do
    let complex = ESeq (EPar wNoOp wPhen) wNoOp
    applyEventWorkflowPhen complex t0 lit0
      `shouldBe` singletonPhen (epsilon a t0)
