{-# LANGUAGE NamedFieldPuns #-}
module Physics.GravNR
  ( gravNR            -- :: Double -> Double -> [(Component,Double)] -> NRune
  ) where

import Components            (Component)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import NRune
import NState
import ScalarLiteral

-- | 1-D Newtonian gravity kick:
--   pᵢ ← pᵢ + dt · Σⱼ G mᵢ mⱼ (qⱼ-qᵢ) / |qⱼ-qᵢ|³
--
-- Sign convention: attractive, so bodies pull toward each other.
gravNR
  :: Double               -- ^ dt
  -> Double               -- ^ G (use 1.0 for “natural” units)
  -> [(Component, Double)]-- ^ masses (positive)
  -> NRune
gravNR dt g masses =
  let dom      = S.fromList (map fst masses)
      mMap     = M.fromList masses
      ids      = map fst masses

      forceOn i st =
        let mi = mMap M.! i
            qi = lookupPos i st
        in sum
             [ let mj = mMap M.! j
                   qj = lookupPos j st
                   dx = qj - qi
                   f  = g * mi * mj * signum dx / (abs dx ** 2)
               in f
             | j <- ids, j /= i ]

      step dT st =
        foldr
          (\i acc ->
              let dp = dT * forceOn i acc
              in insertMom i (lookupMom i acc + dp) acc)
          st
          ids
  in NR { domainN = dom, stepN = step }
