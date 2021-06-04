module Vec3
  ( Vec3,
    vec3,
    vector,
    x,
    y,
    z,
    v,
    sqrtLength,
    toTuple,
    tupleToV,
  )
where

import Numeric.LinearAlgebra

type Vec3 = Vector R

x, y, z :: Vec3 -> R
x v = v ! 0
y v = v ! 1
z v = v ! 2

vec3 :: R -> R -> R -> Vec3
vec3 x y z = vector [x, y, z]

v :: R -> Vector R
v x = vector [x]

sqrtLength :: Vec3 -> R
sqrtLength v = sqrt $ x v ** 2 + y v ** 2 + z v ** 2

toTuple :: Vec3 -> (R, R, R)
toTuple v = (x v, y v, z v)

tupleToV :: (R, R, R) -> Vec3
tupleToV (x, y, z) = vec3 x y z
