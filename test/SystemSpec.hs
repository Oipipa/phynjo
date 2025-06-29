{-# LANGUAGE DataKinds #-}
module SystemSpec (spec) where

import Test.Hspec
import Body
import System
import UnitLiteral (keysU)
import Numeric.Units.Dimensional.Prelude
         ( (*~), kilo, gram )

spec :: Spec
spec = describe "System" $ do
  let b1 = mkBody "a" (1 *~ kilo gram) (-1)
      b2 = mkBody "b" (1 *~ kilo gram)   1

  it "bodies aggregates two masses" $ do
    let sys = bodies [b1, b2]
    length (keysU (sMass sys)) `shouldBe` 2

  it "<+> merges disjoint systems" $ do
    let s1 = bodies [b1]
        s2 = bodies [b2]
    fmap (length . keysU . sMass) (s1 <+> s2) `shouldBe` Just 2

  it "<+> fails on overlap" $ do
    let dup = bodies [b1]
    (dup <+> dup) `shouldBe` Nothing
