{-# LANGUAGE DataKinds #-}
module BodySpec (spec) where

import Test.Hspec
import Base.Body
import Base.UnitLiteral            (keysU)
import Base.Components             (Component(..))
import Numeric.Units.Dimensional.Prelude
         ( (*~), kilo, gram )

spec :: Spec
spec = describe "Body" $ do
  let b = mkBody "bob" (1 *~ kilo gram) 0

  it "mkBody sets identifier" $
       ident b `shouldBe` AtomicC "bob"

  it "mass literal has exactly one entry" $
       length (keysU (mass b)) `shouldBe` 1
