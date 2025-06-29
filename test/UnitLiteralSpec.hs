{-# LANGUAGE DataKinds #-}
module UnitLiteralSpec (spec) where

import Test.Hspec
import Components               (Component(..))
import UnitLiteral
import Numeric.Units.Dimensional.Prelude
         ( (*~), metre, newton, kilo, gram, one, Quantity )
import qualified Numeric.Units.Dimensional.Prelude as D
import qualified Data.Map.Strict as M

spec :: Spec
spec = describe "UnitLiteral" $ do
  let a  = AtomicC "a"
      b  = AtomicC "b"
      xa = 3 *~ metre
      xb = 5 *~ metre
      _ma = 2 *~ (kilo gram)
      _f  = 4 *~ newton
      d2  :: Quantity D.DOne Double
      d2  = 2 *~ one         -- dimension-less factor 2

  it "singleton / lookup round-trip" $
    lookupU a (singletonU a xa) `shouldBe` Just xa

  it "scaleU multiplies every value" $
    let lit  = singletonU a xa
        lit' = scaleU d2 lit
    in lookupU a lit' `shouldBe` Just (d2 D.* xa)

  it "mergeU sums overlapping keys" $
    let l1 = singletonU a xa
        l2 = singletonU a xb
        ULit m = mergeU l1 l2
    in m `shouldBe` M.fromList [(a, xa D.+ xb)]

  it "disjointMergeU succeeds on disjoint keys" $
    disjointMergeU (singletonU a xa) (singletonU b xb)
      `shouldBe` Just (ULit (M.fromList [(a,xa),(b,xb)]))

  it "disjointMergeU fails on overlap" $
    disjointMergeU (singletonU a xa) (singletonU a xb) `shouldBe` Nothing
