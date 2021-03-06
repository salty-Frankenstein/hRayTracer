{-# LANGUAGE MultiWayIf #-}
module Main where

import Camera
import qualified Common as C
import Control.Concurrent
import Control.Monad.Reader
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

nx = 800
ny = 400
ns = 100

randomScene :: IO HB.Hitable
randomScene = do
  resRef <- newIORef []
  rRef <- newRURef (randFast rMax)
  let push x = modifyIORef resRef (x :)
      getRand = getRU rRef
  push $ sphere (V3.vec3 0 (-1000) 0) 1000 (lambertian (V3.vec3 0.5 0.5 0.5))
  forM_ [-11 .. 10] $ \a -> do
    forM_ [-11 .. 10] $ \b -> do
      chooseMat <- getRand
      x <- getRand
      y <- getRand
      let center = V3.vec3 (a + 0.9 * x) 0.2 (b + 0.9 * y)
      when (V3.length (center - V3.vec3 4 0.2 0) > 0.9) $ do
        if
            | chooseMat < 0.8 -> do
              let mul = (*) <$> getRand <*> getRand
              mat <- lambertian <$> liftM3 V3.vec3 mul mul mul
              push $ sphere center 0.2 mat
            | chooseMat < 0.95 -> do
              let f = (\x -> 0.5 * (1 + x)) <$> getRand
              mat <- metal <$> liftM3 V3.vec3 f f f <*> ((0.5 *) <$> getRand)
              push $ sphere center 0.2 mat
            | otherwise -> push $ sphere center 0.2 (dielectric 1.5)
  push $ sphere (V3.vec3 0 1 0) 1 (dielectric 1.5)
  push $ sphere (V3.vec3 (-4) 1 0) 1 (lambertian (V3.vec3 0.4 0.2 0.1))
  push $ sphere (V3.vec3 4 1 0) 1 (metal (V3.vec3 0.7 0.6 0.5) 0)
  hitableList <$> readIORef resRef

color :: R.Ray -> Int -> C.App V3.Vec3
color r depth = do
  C.Env ruRef rRef _ world _ <- ask
  let (res, hrec@(HB.HitRec t p normal material)) = HB.runHit world r 0.001 1000000
  if res then do
      if depth < 50 then do
        scattered <- liftIO $ newIORef r -- TODO
        attenuation <- liftIO $ newIORef (V3.v 0)
        sres <- liftIO $ HB.runMaterial material r hrec attenuation scattered rRef ruRef
        if sres then do
          a <- liftIO $ readIORef attenuation
          s <- liftIO $ readIORef scattered
          (a *) <$> color s (depth+1)
        else return $ V3.vec3 0 0 0
      else return $ V3.vec3 0 0 0
  else return $ V3.v (1 - t2) * V3.vec3 1 1 1 + V3.v t2 * V3.vec3 0.5 0.7 1.0
  where
    unitDir = normalize (R.direction r)
    t2 = 0.5 * (V3.y unitDir + 1)

colorAt :: Int -> Int -> C.App V3.Vec3
colorAt i j = do
  C.Env ruRef rRef cam world _ <- ask
  colRef <- liftIO $ newIORef (V3.vec3 0 0 0)
  colorAt' colRef
  liftIO $ readIORef colRef
  where
    colorAt' colRef = do
      cam <- asks C.camera
      r <- liftIO $ randPair ns
      U.forM_ r $ \(di, dj) -> do
        let u = (fromIntegral i + di) / fromIntegral nx
            v = (fromIntegral j + dj) / fromIntegral ny
        r <- liftIO $ getRay cam u v
        c <- color r 0
        liftIO $ modifyIORef colRef (+ c)

main :: IO ()
main = do
  fh <- openFile "out.ppm" WriteMode
  hPutStr fh $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255\n"
  ruRef <- newRURef (randomInUnitSphere rMax)
  rRef <- newRURef (randFast rMax)

  let lookfrom = V3.vec3 13 2 3
      lookat = V3.vec3 0 0 0
      distToFocus = 10
      aspect = fromIntegral nx / fromIntegral ny
      aperture = 0.1
  cam <- camera lookfrom lookat (V3.vec3 0 1 0) 20 aspect aperture distToFocus
  world <- randomScene
  runReaderT 
    (forM_ [ny -1, ny -2 .. 0] $ \j -> do
       liftIO $ print j
       forM_ [0 .. nx -1] $ \i -> do
         col <- (/ fromIntegral ns) <$> colorAt i j 
         let col' = V3.vec3 (sqrt $ V3.x col) (sqrt $ V3.y col) (sqrt $ V3.z col)
         let ir = truncate (255.99 * V3.x col')
             ig = truncate (255.99 * V3.y col')
             ib = truncate (255.99 * V3.z col')
         liftIO $ hPutStr fh $ show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n") 
    (C.Env ruRef rRef cam world fh)
  hClose fh