{-# LANGUAGE ExistentialQuantification #-}
module Hitable where

import Control.Monad.State
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Numeric.LinearAlgebra
import qualified Ray as R
import qualified Vec3 as V3

newtype Material = Material
  { runMaterial ::
      R.Ray ->                      -- ray in
      HitRec ->                     -- hit record
      IORef V3.Vec3 ->              -- attenuation
      IORef R.Ray ->                -- scattered ray
      IORef (U.Vector (R, R, R)) -> -- random vectors
      IORef Int ->                  -- index
      IO Bool                       -- result
  }

data HitRec = HitRec
  { t :: R,
    p :: V3.Vec3,
    normal :: V3.Vec3,
    material :: Material 
  }
  
rec0 :: HitRec
rec0 = HitRec 0 (V3.v 0) (V3.v 0) undefined 

newtype Hitable = Hitable
  { runHitable ::
      R.Ray ->          -- hit ray
      R ->              -- tMin
      R ->              -- tMax
      State HitRec Bool -- result
  }

runHit :: Hitable -> R.Ray -> R -> R -> (Bool, HitRec)
runHit h r tMin tMax = runState (runHitable h r tMin tMax) rec0
