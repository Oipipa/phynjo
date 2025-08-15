{-# LANGUAGE NamedFieldPuns #-}

module Physics.Forces.ForceNR
  ( forceNR
  ) where

import Physics.Forces.Force    (Force(..))
import NState                  (NState, lookupPos, lookupMom, insertMom)
import NumericRule             (NumericRule(..))
import Components              (Component)
import qualified Data.Set      as S
import qualified Data.Map.Strict as M

forceNR :: Force -> [(Component,Double)] -> NumericRule
forceNR f masses =
  let domain = S.fromList (map fst masses)
      mMap   = M.fromList masses
      ids    = map fst masses
      eps    = 1e-12

      forceOn c st =
        case f of
          Custom ff ->
            let (fx,_,_) = ff st c
            in fx

          Drag gamma ->
            let p = lookupMom c st
                m = mMap M.! c
            in (-gamma) * (p / m)

          Spring i j k rest ->
            let xi   = lookupPos i st
                xj   = lookupPos j st
                dx   = xj - xi
                disp = dx - signum dx * rest
            in if c == i then  k * disp
               else if c == j then (-k) * disp
               else 0

          Gravity g ->
            let mi = mMap M.! c
                qi = lookupPos c st
            in sum
                 [ let mj = mMap M.! cj
                       qj = lookupPos cj st
                       d  = qj - qi
                       r2 = let a = abs d in if a < eps then eps*eps else a*a
                   in g * mi * mj * signum d / r2
                 | cj <- ids, cj /= c
                 ]

      step dt st0 =
        foldr
          (\c acc ->
             let dp = dt * forceOn c acc
                 p0 = lookupMom c acc
             in insertMom c (p0 + dp) acc
          )
          st0
          ids
  in NumericRule { nrDomain = domain, nrStep = step }
