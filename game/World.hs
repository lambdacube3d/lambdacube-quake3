{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module World where

import GHC.Generics (Generic)
import Data.Binary
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
  , mouseU         :: !Float
  , mouseV         :: !Float
  , changeWeapon   :: !(Maybe Items.Weapon)
  , toggleHoldable :: !(Maybe Items.Holdable)
  } deriving (Show, Generic)

data World
  = World -- server side
  { _wEntities  :: ![Entity]  -- snapshot
  , _wVisuals   :: ![Visual]  -- move to client
  , _wInput     :: !Input     -- recived input (for all clients)
  , _wRandomGen :: !PureMT    -- snapshot
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
  , mouseY         = 100 * pi / 2
  , mouseU         = 0
  , mouseV         = 0
  , changeWeapon   = Nothing
  , toggleHoldable = Nothing
  }

data WorldSnapshot
  = WorldSnapshot
  { gameEntities :: ![Entity]
  } deriving (Show, Generic)

instance Binary Input
instance Binary WorldSnapshot
