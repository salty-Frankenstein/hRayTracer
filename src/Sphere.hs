{-# LANGUAGE MultiWayIf #-}
module Sphere where

import Control.Monad.State
import qualified Hitable as HB
import Numeric.LinearAlgebra
import qualified Ray as R
import qualified Vec3 as V3

sphere :: V3.Vec3 -> R -> HB.Material -> HB.Hitable
sphere c r m = HB.Hitable $ hitSphere (Sphere c r m)

hitSphere :: Sphere -> R.Ray -> R -> R -> State HB.HitRec Bool
hitSphere (Sphere cen rad mat) r tMin tMax = do
    let oc = R.origin r - cen
        a = R.direction r <.> R.direction r
        b = oc <.> R.direction r
        c = dot oc oc - rad * rad
        d = b * b - a * c
        temp = (-b - sqrt d) / a
        temp2 = (- b + sqrt d) / a
    if d > 0 then 
      if  | temp < tMax && temp > tMin ->
            do
              let rect = temp
                  recp = R.pointAtParameter r rect
                  recnormal = (recp - cen) / V3.v rad
              put $ HB.HitRec rect recp recnormal mat
              return True
          | temp2 < tMax && temp2 > tMin ->
            do
              let rect = temp2
                  recp = R.pointAtParameter r rect
                  recnormal = (recp - cen) / V3.v rad
              put $ HB.HitRec rect recp recnormal mat
              return True
          | otherwise -> return False
      else return False

data Sphere = Sphere
  { center :: V3.Vec3,
    radius :: R,
    material :: HB.Material
  }
