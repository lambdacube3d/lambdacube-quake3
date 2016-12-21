{-# LANGUAGE TemplateHaskell #-}
module World where

import Lens.Micro.Platform
import System.Random.Mersenne.Pure64

import Visuals
import Entities

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
  , _wMapFile   :: String
  } deriving Show

makeLenses ''World
