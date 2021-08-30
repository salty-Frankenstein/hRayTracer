module Common where

import Control.Monad.Reader
import Numeric.LinearAlgebra
import System.IO
import Camera
import Hitable
import Random 

data Env = Env {
  ruRef :: RURef (R, R, R),
  rRef :: RURef Double,
  camera :: Camera,
  world :: Hitable,
  file :: Handle
}

type App a = ReaderT Env IO a
