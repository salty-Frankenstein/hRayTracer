module HitableList where

import Control.Monad.State
import qualified Hitable as HB
import Numeric.LinearAlgebra
import qualified Ray as R

hitableList :: [HB.Hitable] -> HB.Hitable
hitableList l = HB.Hitable (hitList l)

hitList :: [HB.Hitable] -> R.Ray -> R -> R -> State HB.HitRec Bool
hitList (x:xs) r tMin tMax = do
  saveRec <- get
  res <- HB.runHitable x r tMin tMax
  if res 
    then do
      (HB.HitRec t _ _ _) <- get 
      hitList xs r tMin t
      return True
    else do
      put saveRec
      hitList xs r tMin tMax
hitList [] _ _ _ = return False

