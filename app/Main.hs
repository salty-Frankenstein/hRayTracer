module Main where

import Lib

import Control.Monad.Writer
import Numeric.LinearAlgebra
import qualified Ray as R
import qualified Vec3 as V3

hitSphere :: V3.Vec3 -> R -> R.Ray -> R
hitSphere center radius r 
  | d < 0 = -1.0
  | otherwise = (- b - sqrt d) / (2 * a)
  where 
    oc = R.origin r - center
    a = R.direction r `dot` R.direction r
    b = 2 * oc `dot` R.direction r
    c = dot oc oc - radius * radius
    d = b * b - 4 * a * c

color :: R.Ray -> Vector R
color r 
  | t > 0 = let n = normalize (R.pointAtParameter r t - V3.vec3 0 0 (-1))
             in V3.v 0.5 * V3.vec3 (V3.x n + 1) (V3.y n + 1) (V3.z n + 1) 
  | otherwise = V3.v (1 - t2) * V3.vec3 1 1 1 + V3.v t2 * V3.vec3 0.5 0.7 1.0
  where
    unitDir = normalize (R.direction r) 
    t2 = 0.5 * (V3.y unitDir + 1)
    t = hitSphere (V3.vec3 0 0 (-1)) 0.5 r

writePic :: Writer String ()
writePic = do
  let nx = 200
  let ny = 100
  tell $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255\n"
  let lowerLeftCorner = - V3.vec3 2.0 1.0 1.0
  let horizontal = V3.vec3 4 0 0
  let vertical = V3.vec3 0 2 0
  let origin = V3.vec3 0 0 0
  forM_ [ny -1, ny -2 .. 0] $ \j -> do
    forM_ [0 .. nx -1] $ \i -> do
      let u = fromIntegral i / fromIntegral nx
      let v = fromIntegral j / fromIntegral ny
      let r = R.Ray origin (lowerLeftCorner + V3.v u * horizontal + V3.v v * vertical)
      let col = color r
      let ir = truncate (255.99 * V3.x col)
      let ig = truncate (255.99 * V3.y col)
      let ib = truncate (255.99 * V3.z col)
      tell $ show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n"

main :: IO ()
main = writeFile "out.ppm" (execWriter writePic)
