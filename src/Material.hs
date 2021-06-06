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
    lambertian' rIn hRec@(HB.HitRec t p nor mat) attenuation scattered rRef ruRef = do
      ru <- getRU ruRef
      let target = p + nor + V3.tupleToV ru
      writeIORef scattered (R.Ray p (target - p))
      writeIORef attenuation albedo
      return True

reflect :: V3.Vec3 -> V3.Vec3 -> V3.Vec3
reflect v n = v - V3.v (2 * dot v n) * n

refract :: V3.Vec3 -> V3.Vec3 -> R -> Maybe V3.Vec3
refract v n ni_over_nt 
  | d > 0 = Just $ V3.v ni_over_nt * (uv - n * V3.v dt) - n * V3.v (sqrt d)
  | otherwise = Nothing 
  where
    uv = normalize v
    dt = dot uv n
    d = 1 - ni_over_nt * ni_over_nt * (1-dt*dt)

schlick :: R -> R -> R 
schlick cosine refIdx = 
  let r0 = ((1-refIdx) / (1+refIdx)) ** 2
   in r0 + (1-r0) * (1-cosine) ** 5

metal :: V3.Vec3 -> R -> HB.Material
metal albedo f = HB.Material metal'
  where
    fuzz = if f < 1 then f else 1

    metal' rIn hRec@(HB.HitRec t p nor mat) attenuation scattered rRef ruRef = do
      ru <- getRU ruRef
      let reflected = reflect (normalize . R.direction $ rIn) nor
      writeIORef scattered (R.Ray p (reflected + V3.v fuzz * V3.tupleToV ru))
      writeIORef attenuation albedo
      return $ reflected <.> nor > 0

dielectric :: R -> HB.Material
dielectric refIdx = HB.Material dielectric'
  where
    dielectric' rIn@(R.Ray ori dir) hRec@(HB.HitRec t p nor mat) attenuation scattered rRef ruRef = do
      let reflected = reflect dir nor
      writeIORef attenuation (V3.vec3 1 1 1)
      case refract dir outwardNormal ni_over_nt of
        Just refracted -> do
          let reflectProb = schlick cosine refIdx
          r <- getRU rRef
          if r < reflectProb 
            then writeIORef scattered (R.Ray p reflected)
            else writeIORef scattered (R.Ray p refracted)
        Nothing -> do
          writeIORef scattered (R.Ray p reflected)
      return True
      where
        (outwardNormal, ni_over_nt, cosine) = 
            if dir <.> nor > 0 
              then (-nor, refIdx, refIdx * dot dir nor / V3.length dir)
              else (nor, 1 / refIdx, - dot dir nor / V3.length dir)