module Material where

import Data.IORef
import qualified Data.Vector.Unboxed as U
import qualified Hitable as HB
import Numeric.LinearAlgebra
import Random
import qualified Ray as R
import qualified Vec3 as V3

lambertian :: V3.Vec3 -> HB.Material
lambertian albedo = HB.Material lambertian'
  where
    lambertian' rIn hRec@(HB.HitRec t p nor mat) attenuation scattered ruRef idxRef = do
      ruv <- readIORef ruRef
      idx <- readIORef idxRef
      case ruv U.!? idx of
        Just ru -> do
          let target = p + nor + V3.tupleToV ru
          modifyIORef idxRef (+ 1)
          writeIORef scattered (R.Ray p (target - p))
          writeIORef attenuation albedo
          return True
        Nothing -> do
          writeIORef idxRef 0
          lambertian' rIn hRec attenuation scattered ruRef idxRef

metal :: V3.Vec3 -> HB.Material
metal albedo = HB.Material metal'
  where
    reflect v n = v - V3.v (2 * dot v n) * n

    metal' rIn hRec@(HB.HitRec t p nor mat) attenuation scattered ruRef idxRef = do
      ruv <- readIORef ruRef
      idx <- readIORef idxRef
      case ruv U.!? idx of
        Just ru -> do
          let reflected = reflect (normalize . R.direction $ rIn) nor
          modifyIORef idxRef (+ 1)
          writeIORef scattered (R.Ray p reflected)
          writeIORef attenuation albedo
          return $ reflected `dot` nor > 0
        Nothing -> do
          writeIORef idxRef 0
          metal' rIn hRec attenuation scattered ruRef idxRef
          