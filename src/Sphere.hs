{-# LANGUAGE MultiWayIf #-}
module Sphere where

import qualified Hitable as HB
import Numeric.LinearAlgebra
import Control.Monad.State
import qualified Ray as R
import qualified Vec3 as V3

data Sphere = Sphere {center :: V3.Vec3, radius :: R}

instance HB.Hitable_ Sphere where
  hit (Sphere cen rad) r tMin tMax = do
    let oc = R.origin r - cen
        a = R.direction r `dot` R.direction r
        b = oc `dot` R.direction r
        c = dot oc oc - rad * rad
        d = b * b - a * c
        temp = (-b - sqrt d) / a
        temp2 = (- b + sqrt d) / a
    if d > 0 then (
      if {| temp < tMax && temp > tMin ->
            do
              let rect = temp
                  recp = R.pointAtParameter r rect
                  recnormal = (recp - cen) / V3.v rad
              put $ HB.HitRec rect recp recnormal 
              return True ;
          | temp2  < tMax && temp2 > tMin ->
            do
              let rect = temp2
                  recp = R.pointAtParameter r rect
                  recnormal = (recp - cen) / V3.v rad
              put $ HB.HitRec rect recp recnormal 
              return True ;
          | otherwise -> return False 

      }) else return False


