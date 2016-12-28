{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module Visuals where

import Data.Vect
import Lens.Micro.Platform
import Control.Monad

-- visuals for game graphics
data Particle
  = Particle
  { _vpPosition   :: Vec3
  , _vpDirection  :: Vec3
  , _vpLifeTime   :: Float
  } deriving Show

data Visual
  = VParticle Particle
  deriving Show

concat <$> mapM makeLenses [''Particle]
