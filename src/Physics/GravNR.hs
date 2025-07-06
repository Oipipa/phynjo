{-# LANGUAGE NamedFieldPuns #-}
module Physics.GravNR
  ( gravNR     -- :: Double -> Double -> [(Component,Double)] -> NumericRule
  ) where

import           Components         (Component)
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S

import           NumericRule        (NumericRule(..))
import           NState             (NState, lookupPos, lookupMom, insertMom)

gravNR
  :: Double                  -- ^ Δt
  -> Double                  -- ^ G (gravitational constant)
  -> [(Component,Double)]    -- ^ list of (body, mass in kg)
  -> NumericRule
gravNR dt g masses =
  let domain = S.fromList (map fst masses)
      mMap   = M.fromList masses
      ids    = map fst masses

      -- net force on body i in current state
      forceOn i st =
        let mi = mMap M.! i
            qi = lookupPos i st
        in sum
             [ let mj = mMap M.! j
                   qj = lookupPos j st
                   dx = qj - qi
               in g * mi * mj * signum dx / (abs dx ** 2)
             | j <- ids, j /= i
             ]

      step _dt st0 =
        foldr
          (\i acc ->
             let dp = dt * forceOn i acc
                 p0 = lookupMom i acc
             in insertMom i (p0 + dp) acc
          )
          st0
          ids

  in NumericRule { nrDomain = domain, nrStep = step }
