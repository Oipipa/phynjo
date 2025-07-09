module Physics.Sim.ComposeSpec (spec) where

import           Test.Hspec
import qualified Data.Map.Strict          as M
import qualified Data.Set                 as S

import           Physics.Sim.Compose
import           Physics.RigidBodyUtilities.Rigid3DNR        (RRune (..), applyRRuneWorld)
import           Physics.RigidBodyUtilities.RigidState
import           Physics.Forces.Force3D
import           Components               (Component (..))

cA, cB :: Component
cA = AtomicC "A"
cB = AtomicC "B"

iUnit :: ((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))
iUnit = ((1,0,0),(0,1,0),(0,0,1))

constForce :: (Double,Double,Double) -> Force3D
constForce f = Force3D $ \_ _ -> (f,(0,0,0))

runePlus1, runePlus2 :: RRune
runePlus1 =
  let dom = S.singleton cA
      step _ st = st { rsPos = M.adjust (\(x,y,z)->(x,y+1,z)) cA (rsPos st) }
  in RR dom step
runePlus2 =
  let dom = S.singleton cA
      step _ st = st { rsPos = M.adjust (\(x,y,z)->(x,y+2,z)) cA (rsPos st) }
  in RR dom step

spec :: Spec
spec = do
  describe "composeRRune" $ do
    it "applies runes in order" $ do
      let st0 = emptyRigid { rsPos = M.singleton cA (0,0,0) }
          cr  = composeRRune [runePlus1, runePlus2]
          st1 = applyRRuneWorld cr 0 st0
          (_,y,_) = rsPos st1 M.! cA
      y `shouldBe` 3        -- 0 +1 +2

    it "union of domains equals domain of composite" $ do
      let dom = domainR (composeRRune [runePlus1, runePlus2])
      dom `shouldBe` S.singleton cA

  describe "forceRune" $ do
    it "sums multiple Force3D terms before kicking" $ do
      -- mass 1 kg, so Δv = dt * ΣF / m ; with dt=1, expect +3 on x
      let masses     = [(cA, 1)]
          inertias   = [(cA, iUnit)]
          forces     = [constForce (1,0,0), constForce (2,0,0)]
          fr         = forceRune masses inertias forces
          st0        = emptyRigid { rsVel = M.singleton cA (0,0,0) }
          st1        = applyRRuneWorld fr 1 st0   -- dt = 1 s
          (vx,_,_)   = rsVel st1 M.! cA
      vx `shouldBe` 3
