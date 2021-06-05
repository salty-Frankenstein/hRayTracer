module Camera where

import Data.IORef
import qualified Data.Vector.Unboxed as U
import qualified Hitable as HB
import Numeric.LinearAlgebra
import Random
import qualified Ray as R
import qualified Vec3 as V3

data Camera = Camera
  { origin :: V3.Vec3,
    lowerLeftCorner :: V3.Vec3,
    horizontal :: V3.Vec3,
    vertical :: V3.Vec3,
    u :: V3.Vec3,
    v :: V3.Vec3,
    w :: V3.Vec3,
    lensRadius :: R,
    ruRef :: IORef (U.Vector (R, R, R)),
    idxRef :: IORef Int
  }

camera :: V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> R -> R -> R -> R -> IO Camera
camera lookfrom lookat vup vfov aspect aperture focusDist = do
  ruRef <- randomInUnitDisk rMax >>= newIORef
  idxRef <- newIORef 0
  let theta = vfov * pi / 180
      halfHeight = tan (theta / 2)
      halfWidth = aspect * halfHeight
      ori = lookfrom
      w = normalize (lookfrom - lookat)
      u = normalize (vup `cross` w)
      v = w `cross` u
      lower = ori - V3.v (halfWidth * focusDist) * u 
            - V3.v (halfHeight * focusDist) * v - V3.v focusDist * w
      hor = V3.v (2 * halfWidth * focusDist) * u
      ver = V3.v (2 * halfHeight * focusDist) * v
  return $ Camera ori lower hor ver u v w (aperture / 2) ruRef idxRef

getRay :: Camera -> R -> R -> IO R.Ray
getRay (Camera ori low hor ver u v w lensR ruRef idxRef) s t = do
  r <- getRU ruRef idxRef
  let rd = V3.v lensR * V3.tupleToV r
      offset = u * V3.v (V3.x rd) + v * V3.v (V3.y rd)
  return $ R.Ray (ori + offset) (low + V3.v s * hor + V3.v t * ver - ori - offset)
