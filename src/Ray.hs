module Ray where

import Vec3
data Ray = Ray {a :: Vec3, b :: Vec3}
origin = a
direction = b

pointAtParameter :: Ray -> R -> Vec3
pointAtParameter (Ray a b) t = a + vector [t] * b
