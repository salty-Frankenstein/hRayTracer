module Camera where

import qualified Hitable as HB
import Numeric.LinearAlgebra
import qualified Ray as R
import qualified Vec3 as V3

data Camera = Camera
  { origin :: V3.Vec3,
    lowerLeftCorner :: V3.Vec3,
    horizontal :: V3.Vec3,
    vertical :: V3.Vec3
  }

camera0 = Camera (V3.vec3 0 0 0) (- V3.vec3 2 1 1) (V3.vec3 4 0 0) (V3.vec3 0 2 0)

getRay :: Camera -> R -> R -> R.Ray 
getRay (Camera ori low hor ver) u v = R.Ray ori (low + V3.v u * hor + V3.v v * ver - ori)
