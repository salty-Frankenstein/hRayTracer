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

camera :: V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> R -> R -> Camera
camera lookfrom lookat vup vfov aspect = 
  let theta = vfov * pi / 180
      halfHeight = tan (theta / 2)
      halfWidth = aspect * halfHeight
      ori = lookfrom
      w = normalize (lookfrom - lookat)
      u = normalize (vup `cross` w)
      v = w `cross` u
      lower = ori - V3.v halfWidth * u - V3.v halfHeight * v - w
      hor = V3.v (2*halfWidth) * u
      ver = V3.v (2*halfHeight) * v
   in Camera ori lower hor ver

getRay :: Camera -> R -> R -> R.Ray 
getRay (Camera ori low hor ver) u v = R.Ray ori (low + V3.v u * hor + V3.v v * ver - ori)
