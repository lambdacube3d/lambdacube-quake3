module GameEngine.Data.GameCharacter where

import LambdaCube.Linear (V3(..))
import Data.Map

data Animation
  = Animation
  { aFirstFrame :: !Int
  , aNumFrames  :: !Int
  , aLoopFrames :: !Int  -- 0 to numFrames
  , aFrameLerp  :: !Int  -- msec between frames
  , aIntialLerp :: !Int  -- msec to get to first frame
  , aReversed   :: !Bool -- true if animation is reversed
  , aFlipFlop   :: !Bool -- true if animation should flipflop back to base
  }
  deriving (Show, Eq, Ord)

data FootStep
  = FOOTSTEP_NORMAL
  | FOOTSTEP_BOOT
  | FOOTSTEP_FLESH
  | FOOTSTEP_MECH
  | FOOTSTEP_ENERGY
  | FOOTSTEP_METAL
  | FOOTSTEP_SPLASH
  deriving (Show, Eq, Ord)

data Gender
  = GENDER_MALE
  | GENDER_FEMALE
  | GENDER_NEUTER
  deriving (Show, Eq, Ord)

data AnimationType
  = BOTH_DEATH1
  | BOTH_DEAD1
  | BOTH_DEATH2
  | BOTH_DEAD2
  | BOTH_DEATH3
  | BOTH_DEAD3

  | TORSO_GESTURE

  | TORSO_ATTACK
  | TORSO_ATTACK2

  | TORSO_DROP
  | TORSO_RAISE

  | TORSO_STAND
  | TORSO_STAND2

  | LEGS_WALKCR
  | LEGS_WALK
  | LEGS_RUN
  | LEGS_BACK
  | LEGS_SWIM

  | LEGS_JUMP
  | LEGS_LAND

  | LEGS_JUMPB
  | LEGS_LANDB

  | LEGS_IDLE
  | LEGS_IDLECR

  | LEGS_TURN

  | TORSO_GETFLAG
  | TORSO_GUARDBASE
  | TORSO_PATROL
  | TORSO_FOLLOWME
  | TORSO_AFFIRMATIVE
  | TORSO_NEGATIVE

  | LEGS_BACKCR
  | LEGS_BACKWALK
  | FLAG_RUN
  | FLAG_STAND
  | FLAG_STAND2RUN
  deriving (Show, Eq, Ord, Bounded, Enum)

data Character
  = Character
  { animationMap  :: !(Map AnimationType Animation)
  , gender        :: !Gender
  , footStep      :: !FootStep
  , headOffset    :: !(V3 Float)
  , fixedLegs     :: !Bool
  , fixedTorso    :: !Bool
  }
  deriving (Show, Eq, Ord)

