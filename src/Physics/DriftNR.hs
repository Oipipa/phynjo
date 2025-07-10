{-# LANGUAGE NamedFieldPuns #-}
module Physics.DriftNR
  ( driftNR        -- :: [(Component,Double)] -> NumericRule
  ) where

import           Components         (Component)
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S

import           NumericRule        (NumericRule(..))
import           NState             (NState, lookupPos, lookupMom, insertPos)

driftNR :: [(Component, Double)] -> NumericRule
driftNR masses =
  let dom     = S.fromList (map fst masses)
      massMap = M.fromList masses  -- Component -> m_i
      step dt st0 =
        foldr
          (\c acc ->
              let m   = massMap M.! c
                  qi  = lookupPos c acc
                  pi  = lookupMom c acc
                  qi' = qi + dt * pi / m
              in insertPos c qi' acc)
          st0
          (map fst masses)
  in NumericRule { nrDomain = dom, nrStep = step }
