module Vec3
  ( Vec3,
    vec3,
    vector,
    x,
    y,
    z,
    v,
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
