{-# LANGUAGE NamedFieldPuns #-}

module Physics.Forces.GravNR
  ( gravNR
  ) where

import           Components         (Component)
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S

import           NumericRule        (NumericRule(..))
import           NState             (NState, lookupPos, lookupMom, insertMom)

gravNR :: Double -> Double -> [(Component,Double)] -> NumericRule
gravNR dt g masses =
  let domain = S.fromList (map fst masses)
      mMap   = M.fromList masses
      ids    = map fst masses
      eps    = 1e-12

      forceOn i st =
        let mi = mMap M.! i
            qi = lookupPos i st
        in sum
             [ let mj = mMap M.! j
                   qj = lookupPos j st
                   d  = qj - qi
                   r2 = let a = abs d in if a < eps then eps*eps else a*a
               in g * mi * mj * signum d / r2
             | j <- ids, j /= i
             ]

      step _ st0 =
        foldr
          (\i acc ->
             let dp = dt * forceOn i acc
                 p0 = lookupMom i acc
             in insertMom i (p0 + dp) acc
          )
          st0
          ids
  in NumericRule { nrDomain = domain, nrStep = step }
