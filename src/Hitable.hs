{-# LANGUAGE ExistentialQuantification #-}
module Hitable where

import Numeric.LinearAlgebra
import Control.Monad.State
import qualified Ray as R
import qualified Vec3 as V3

data HitRec = HitRec {t :: R, p :: V3.Vec3, normal :: V3.Vec3}

rec0 = HitRec 0 (V3.v 0) (V3.v 0)

newtype Hitable = Hitable {runHitable :: R.Ray -> R -> R -> State HitRec Bool}

runHit :: Hitable -> R.Ray -> R -> R -> (Bool, HitRec)
runHit h r tMin tMax = runState (runHitable h r tMin tMax) rec0
