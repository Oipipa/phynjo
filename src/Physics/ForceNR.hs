{-# LANGUAGE NamedFieldPuns #-}

module Physics.ForceNR
  ( forceNR
  ) where

import Physics.Force    (Force(..))
import NState           (NState, lookupPos, lookupMom, insertMom)
import NRune            (NRune(..))
import Components       (Component)
import qualified Data.Set          as S
import qualified Data.Map.Strict   as M

-- | Build an NRune that applies a force to each component,
--   updating pᵢ ← pᵢ + Δt * Fᵢ, where Fᵢ is the x‐component
--   of the total force on body i.
forceNR
  :: Force                -- ^ force to apply
  -> [(Component,Double)] -- ^ list of (body, mass in kg)
  -> NRune
forceNR f masses =
  let domain  = S.fromList (map fst masses)
      mMap    = M.fromList masses

      step dt st0 = foldr update st0 masses
        where
          update (c,m) acc =
            let p0 = lookupMom c acc
                fx = case f of
                  -- user‐supplied custom vector‐field
                  Custom ff ->
                    let (fx',_,_) = ff acc c
                    in fx'

                  -- linear drag: F = –γ v = –γ (p/m)
                  Drag γ    ->
                    let v = p0 / m
                    in - γ * v

                  -- Hooke spring between i and j: 
                  --   F_i =  k·( (x_j - x_i) - sign(dx)*rest )
                  --   F_j = -F_i
                  Spring i j k rest ->
                    let xi   = lookupPos i acc
                        xj   = lookupPos j acc
                        dx   = xj - xi
                        disp = dx - signum dx * rest
                    in if c == i then  k * disp
                       else if c == j then -k * disp
                       else 0

                  -- 1-D Newtonian gravity kick
                  Gravity g ->
                    let mi    = mMap M.! c
                        qi    = lookupPos c acc
                        -- sum over j≠i of G mi mj sign(dx)/|dx|^2
                        fsum  = sum
                          [ let mj = mMap M.! cj
                                qj = lookupPos cj acc
                                dx = qj - qi
                            in g * mi * mj * signum dx / (abs dx ** 2)
                          | (cj,_) <- masses, cj /= c
                          ]
                    in fsum

                p' = p0 + dt * fx
            in insertMom c p' acc

  in NR { domainN = domain, stepN = step }
