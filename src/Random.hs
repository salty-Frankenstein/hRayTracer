{-# LANGUAGE ScopedTypeVariables #-}
module Random where

import qualified Data.Vector.Unboxed as U
import Control.DeepSeq
import System.Random.MWC
import Control.Monad.ST
import Control.Monad
import qualified Vec3 as V3
import Numeric.LinearAlgebra


randFast :: Int -> IO (U.Vector Double)
randFast n = do
  vs <- withSystemRandom $
    \(gen::GenST s) -> uniformVector gen n :: ST s (U.Vector Double)
  return $ U.force (vs :: (U.Vector Double))

-- randomInUnitSphere :: Int -> IO [(R,R,R)]
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
      | V3.sqrtLength (getVec p) < 1 = True
      | otherwise = False
    getVec (x,y,z) = V3.v 2 * V3.vec3 x y z - V3.vec3 1 1 1

randPair :: Int -> IO (U.Vector (Double, Double))
randPair n = liftM2 U.zip (randFast n) (randFast n)
