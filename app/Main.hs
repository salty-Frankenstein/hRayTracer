module Main where

import Camera
import Control.Monad.Writer
import Data.IORef
import qualified Hitable as HB
import HitableList
import Numeric.LinearAlgebra
import qualified Ray as R
import Sphere
import System.Random
import qualified Vec3 as V3

color :: R.Ray -> HitableList -> V3.Vec3
color r world
  | res = V3.v 0.5 * V3.vec3 (V3.x normal + 1) (V3.y normal + 1) (V3.z normal + 1)
  | otherwise = V3.v (1 - t2) * V3.vec3 1 1 1 + V3.v t2 * V3.vec3 0.5 0.7 1.0
  where
    (res, HB.HitRec t p normal) = HB.runHit (HB.hit world r 0 1000000) HB.rec0
    unitDir = normalize (R.direction r)
    t2 = 0.5 * (V3.y unitDir + 1)

randomStream :: IO [R]
randomStream = randomRs (0, 1) <$> newStdGen

writePic :: WriterT String IO ()
writePic = do
  let nx = 200
      ny = 100
      ns = 100
  tell $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255\n"
  let s1 = Sphere (V3.vec3 0 0 (-1)) 0.5
      s2 = Sphere (V3.vec3 0 (-100.5) (-1)) 100
      world = HitableList $ map HB.Hitable [s1, s2]

  colRef <- liftIO $ newIORef (V3.vec3 0 0 0)
  forM_ [ny -1, ny -2 .. 0] $ \j -> do
    forM_ [0 .. nx -1] $ \i -> do
      liftIO $ writeIORef colRef (V3.vec3 0 0 0)
      r <- liftIO $ liftM2 zip randomStream randomStream
      forM_ (take ns r) $ \(di, dj) -> do
        let u = (fromIntegral i + di) / fromIntegral nx
            v = (fromIntegral j + dj) / fromIntegral ny
            r = getRay camera0 u v
        liftIO $ modifyIORef colRef (+ color r world)
        
      col <- liftIO $ (/ fromIntegral ns) <$> readIORef colRef
      let ir = truncate (255.99 * V3.x col)
          ig = truncate (255.99 * V3.y col)
          ib = truncate (255.99 * V3.z col)
      tell $ show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n"

main :: IO ()
main =
  execWriterT writePic
    >>= writeFile "out.ppm"
