module Main where

import Camera
import Control.Concurrent
import Control.Monad.Writer
import Data.IORef
import qualified Data.Vector.Unboxed as U
import qualified Hitable as HB
import HitableList
import Material
import Numeric.LinearAlgebra
import Random
import qualified Ray as R
import Sphere
import System.IO
import qualified Vec3 as V3

rMax = 200000
nx = 200
ny = 100
ns = 100
world =
  hitableList
    [ sphere (V3.vec3 0 0 (-1)) 0.5 (lambertian (V3.vec3 0.1 0.2 0.5)),
      sphere (V3.vec3 0 (-100.5) (-1)) 100 (lambertian (V3.vec3 0.8 0.8 0)),
      sphere (V3.vec3 1 0 (-1)) 0.5 (metal (V3.vec3 0.8 0.6 0.2) 0.3),
      sphere (V3.vec3 (-1) 0 (-1)) 0.5 (dielectric 1.5),
      sphere (V3.vec3 (-1) 0 (-1)) (-0.45) (dielectric 1.5)
    ]

color :: R.Ray -> IORef (U.Vector R) -> IORef (U.Vector (R, R, R)) -> IORef Int -> HB.Hitable -> Int -> IO V3.Vec3
color r rRef ruRef idxRef world depth
  | res = do
      if depth < 50 then do
        scattered <- newIORef r -- TODO
        attenuation <- newIORef (V3.v 0)
        sres <- HB.runMaterial material r hrec attenuation scattered rRef ruRef idxRef
        if sres then do
          a <- readIORef attenuation
          s <- readIORef scattered
          (a *) <$> color s rRef ruRef idxRef world (depth+1)
        else return $ V3.vec3 0 0 0
      else return $ V3.vec3 0 0 0
  | otherwise = return $ V3.v (1 - t2) * V3.vec3 1 1 1 + V3.v t2 * V3.vec3 0.5 0.7 1.0
  where
    (res, hrec@(HB.HitRec t p normal material)) = HB.runHit world r 0.001 1000000
    unitDir = normalize (R.direction r)
    t2 = 0.5 * (V3.y unitDir + 1)

colorAt i j rRef ruRef idxRef = do
  colRef <- newIORef (V3.vec3 0 0 0)
  colorAt' colRef
  readIORef colRef
  where
    colorAt' colRef = do
      r <- randPair ns
      U.forM_ r $ \(di, dj) -> do
        let u = (fromIntegral i + di) / fromIntegral nx
            v = (fromIntegral j + dj) / fromIntegral ny
            r = getRay camera0 u v
        c <- color r rRef ruRef idxRef world 0
        modifyIORef colRef (+ c)

main :: IO ()
main = do
  fh <- openFile "out.ppm" WriteMode
  hPutStr fh $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255\n"
  ruRef <- randomInUnitSphere rMax >>= newIORef
  rRef <- randFast rMax >>= newIORef
  idxRef <- newIORef 0
  forM_ [ny -1, ny -2 .. 0] $ \j -> do
    print j
    forM_ [0 .. nx -1] $ \i -> do
      col <- (/ fromIntegral ns) <$> colorAt i j rRef ruRef idxRef
      let col' = V3.vec3 (sqrt $ V3.x col) (sqrt $ V3.y col) (sqrt $ V3.z col)
      let ir = truncate (255.99 * V3.x col')
          ig = truncate (255.99 * V3.y col')
          ib = truncate (255.99 * V3.z col')
      hPutStr fh $ show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n"
  hClose fh
