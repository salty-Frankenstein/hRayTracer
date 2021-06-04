{-# LANGUAGE LambdaCase #-}
module Main where

import Camera
import Control.Monad.Writer
import Data.IORef
import qualified Hitable as HB
import HitableList
import Numeric.LinearAlgebra
import qualified Ray as R
import Sphere
import qualified Vec3 as V3
import Random
import qualified Data.Vector.Unboxed as U
import System.IO

rMax = 200
nx = 200
ny = 100
ns = 100
s1 = Sphere (V3.vec3 0 0 (-1)) 0.5
s2 = Sphere (V3.vec3 0 (-100.5) (-1)) 100
world = HitableList $ map HB.Hitable [s1, s2]

color :: R.Ray -> IORef (U.Vector (R, R, R)) -> IORef Int -> HitableList -> IO V3.Vec3
color r ruRef idxRef world
  | res = do
      ruv <- readIORef ruRef
      idx <- readIORef idxRef
      case ruv U.!? idx of
        Just ru -> do
          let target = p + normal + V3.tupleToV ru
          modifyIORef idxRef (+1)
          (V3.v 0.5 *) <$> color (R.Ray p (target - p)) ruRef idxRef world 
        Nothing -> do
          writeIORef idxRef 0
          color r ruRef idxRef world
  | otherwise = return $ V3.v (1 - t2) * V3.vec3 1 1 1 + V3.v t2 * V3.vec3 0.5 0.7 1.0
  where
    (res, HB.HitRec t p normal) = HB.runHit (HB.hit world r 0 1000000) HB.rec0
    unitDir = normalize (R.direction r)
    t2 = 0.5 * (V3.y unitDir + 1)
-- color r [] world = do
--   print "here\n"
--   ru <- randomInUnitSphere ns 
--   color r ru world

colorAt i j ruRef idxRef = do
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
        c <- color r ruRef idxRef world
        modifyIORef colRef (+c)

main :: IO ()
main = do
  fh <- openFile "out.ppm" WriteMode 
  hPutStr fh $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255\n"
  ruRef <- randomInUnitSphere rMax >>= newIORef
  idxRef <- newIORef 0
  forM_ [ny -1, ny -2 .. 0] $ \j -> do
    print j
    forM_ [0 .. nx -1] $ \i -> do
      col <- (/ fromIntegral ns) <$> colorAt i j ruRef idxRef
      let col'  = V3.vec3 (sqrt $ V3.x col) (sqrt $ V3.y col) (sqrt $ V3.z col)
      let ir = truncate (255.99 * V3.x col')
          ig = truncate (255.99 * V3.y col')
          ib = truncate (255.99 * V3.z col')
      hPutStr fh $ show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n"
  hClose fh
