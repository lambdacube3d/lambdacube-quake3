{-# LANGUAGE TemplateHaskell #-}
module World where

import Lens.Micro.Platform
import System.Random.Mersenne.Pure64

import Visuals
import Entities
import qualified Items

data Input
  = Input
  { forwardmove    :: !Float
  , sidemove       :: !Float
  , shoot          :: !Bool
  , dtime          :: !Float
  , time           :: !Float
  , mouseX         :: !Float
  , mouseY         :: !Float
  , windowWidth    :: !Int
  , windowHeight   :: !Int
  , changeWeapon   :: !(Maybe Items.Weapon)
  , toggleHoldable :: !(Maybe Items.Holdable)
  } deriving Show

data World
  = World
  { _wEntities  :: ![Entity]
  , _wVisuals   :: ![Visual]
  , _wInput     :: !Input
  , _wRandomGen :: !PureMT
  , _wMapFile   :: !String
  } deriving Show

makeLenses ''World

initWorld ents mapfile random = World
  { _wEntities  = ents
  , _wVisuals   = []
  , _wInput     = initInput
  , _wRandomGen = random
  , _wMapFile   = mapfile
  }

initInput = Input
  { forwardmove    = 0
  , sidemove       = 0
  , shoot          = False
  , dtime          = 0
  , time           = 0
  , mouseX         = 0
  , mouseY         = 0
  , windowWidth    = 0
  , windowHeight   = 0
  , changeWeapon   = Nothing
  , toggleHoldable = Nothing
  }
