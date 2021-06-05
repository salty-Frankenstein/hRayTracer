{-# LANGUAGE ScopedTypeVariables #-}
module Random where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Numeric.LinearAlgebra
import System.Random.MWC
import qualified Vec3 as V3


rMax :: Int
rMax = 200000

getRU :: U.Unbox a => IORef (U.Vector a) -> IORef Int -> IO a
getRU ruRef idxRef = do
  ruv <- readIORef ruRef
  idx <- readIORef idxRef
  case ruv U.!? idx of
    Just ru -> do
      modifyIORef idxRef (+ 1)
      return ru
    Nothing -> do
      writeIORef idxRef 0
      putStrLn "reuse random"
      getRU ruRef idxRef

randFast :: Int -> IO (U.Vector Double)
randFast n = do
  vs <- withSystemRandom $
    \(gen::GenST s) -> uniformVector gen n :: ST s (U.Vector Double)
  return $ U.force (vs :: (U.Vector Double))

randomInUnitSphere :: Int -> IO (U.Vector (R, R, R))
randomInUnitSphere n = do
  let len = n * 2
  t <- liftM3 U.zip3 (randFast len) (randFast len) (randFast len)
  let x = U.filter fst $ U.map (\(a,b) -> (legal a, b)) (U.zip t t)
      res = U.map (V3.toTuple.getVec.snd) x
  if U.length x >= n then return res
  else (res U.++) <$> randomInUnitSphere (n - U.length x)
  where
    legal p
      | V3.length (getVec p) ** 2 < 1 = True
      | otherwise = False
    getVec (x,y,z) = V3.v 2 * V3.vec3 x y z - V3.vec3 1 1 1

randomInUnitDisk :: Int -> IO (U.Vector (R, R, R))
randomInUnitDisk n = do
  let len = n * 2
  -- TODO here
  t <- liftM3 U.zip3 (randFast len) (randFast len) (randFast len)
  let x = U.filter fst $ U.map (\(a,b) -> ((legal.getVec) a, b)) (U.zip t t)
      res = U.map (V3.toTuple.getVec.snd) x
  if U.length x >= n then return res
  else (res U.++) <$> randomInUnitSphere (n - U.length x)
  where
    legal p
      | p <.> p < 1 = True
      | otherwise = False
    getVec (x,y,_) = V3.v 2 * V3.vec3 x y 0 - V3.vec3 1 1 0

randPair :: Int -> IO (U.Vector (Double, Double))
randPair n = liftM2 U.zip (randFast n) (randFast n)
