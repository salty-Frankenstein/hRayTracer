module Main where

import Control.Monad.Writer
import Numeric.LinearAlgebra
import qualified Ray as R
import qualified Vec3 as V3
import qualified Hitable as HB
import HitableList
import Sphere

color :: R.Ray -> HitableList -> V3.Vec3
color r world
  | res = V3.v 0.5 * V3.vec3 (V3.x normal + 1) (V3.y normal + 1) (V3.z normal + 1)
  | otherwise = V3.v (1 - t2) * V3.vec3 1 1 1 + V3.v t2 * V3.vec3 0.5 0.7 1.0
  where
    (res, HB.HitRec t p normal) = HB.runHit (HB.hit world r 0 1000000) HB.rec0
    unitDir = normalize (R.direction r)
    t2 = 0.5 * (V3.y unitDir + 1)

writePic :: Writer String ()
writePic = do
  let nx = 200
      ny = 100
  tell $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255\n"
  let lowerLeftCorner = - V3.vec3 2.0 1.0 1.0
      horizontal = V3.vec3 4 0 0
      vertical = V3.vec3 0 2 0
      origin = V3.vec3 0 0 0
      s1 = Sphere (V3.vec3 0 0 (-1)) 0.5
      s2 = Sphere (V3.vec3 0 (-100.5) (-1)) 100
      world = HitableList $ map HB.Hitable [s1, s2]
  forM_ [ny -1, ny -2 .. 0] $ \j -> do
    forM_ [0 .. nx -1] $ \i -> do
      let u = fromIntegral i / fromIntegral nx
          v = fromIntegral j / fromIntegral ny
          r = R.Ray origin (lowerLeftCorner + V3.v u * horizontal + V3.v v * vertical)
          p = R.pointAtParameter r 2
          col = color r world
          ir = truncate (255.99 * V3.x col)
          ig = truncate (255.99 * V3.y col)
          ib = truncate (255.99 * V3.z col)
      tell $ show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n"

main :: IO ()
main = writeFile "out.ppm" (execWriter writePic)
