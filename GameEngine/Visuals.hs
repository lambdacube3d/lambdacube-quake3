{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module GameEngine.Visuals where

import Data.Vect
import Lens.Micro.Platform
import Control.Monad

-- visuals for game graphics
data Particle
  = Particle
  { _vpPosition   :: Vec2
  , _vpDirection  :: Vec2
  , _vpLifeTime   :: Float
  } deriving Show

data Visual
  = VParticle Particle
  deriving Show

concat <$> mapM makeLenses [''Particle]
