{-# LANGUAGE TemplateHaskell #-}
module GameEngine.World where

import Lens.Micro.Platform
import System.Random.Mersenne.Pure64

import GameEngine.Visuals
import GameEngine.Entities

data Input
  = Input
  { forwardmove :: Float
  , rightmove   :: Float
  , sidemove    :: Float
  , shoot       :: Bool
  , dtime       :: Float
  , time        :: Float
  } deriving Show

data World
  = World
  { _wEntities  :: [Entity]
  , _wVisuals   :: [Visual]
  , _wInput     :: Input
  , _wRandomGen :: PureMT
  } deriving Show

makeLenses ''World
