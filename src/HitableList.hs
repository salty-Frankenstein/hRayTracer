module HitableList where

import qualified Hitable as HB
import Control.Monad.State
newtype HitableList = HitableList [HB.Hitable]

instance HB.Hitable_ HitableList where
  hit (HitableList l) r tMin tMax = hit' l r tMin tMax
    where
      hit' (x:xs) r tMin closestSoFar = do
        saveRec <- get
        res <- HB.hit x r tMin closestSoFar
        if res
          then do
            (HB.HitRec t p normal) <- get 
            hit' xs r tMin t
            return True
          else do   -- not hit, recover the rec
            put saveRec
            hit' xs r tMin closestSoFar
      hit' [] _ _ _ = return False
