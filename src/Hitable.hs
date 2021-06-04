{-# LANGUAGE ExistentialQuantification #-}
module Hitable where

import Numeric.LinearAlgebra
import Control.Monad.State
import qualified Ray as R
import qualified Vec3 as V3

data HitRec = HitRec {t :: R, p :: V3.Vec3, normal :: V3.Vec3}

rec0 = HitRec 0 (V3.v 0) (V3.v 0)

class Hitable_ a where
  hit :: a -> R.Ray -> R -> R -> State HitRec Bool

data Hitable = forall a. Hitable_ a => Hitable a
instance Hitable_ Hitable where
  hit (Hitable a) = hit a

runHit = runState
