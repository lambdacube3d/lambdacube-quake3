{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Character where

import Control.Applicative
import Control.Monad
import LambdaCube.Linear
import Data.Map (Map,(!))
import qualified Data.Map as Map

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import ShaderParser (val,kw,float,int,nat,skip)
{-
  TODO:
    done - parse animation data
    - animation playback machinery
    - tag handling: join character according the tag info
    - character action state machine
        add character to storage
        play animation
        update
-}
data Animation
  = Animation
  { aFirstFrame :: Int
  , aNumFrames  :: Int
  , aLoopFrames :: Int  -- 0 to numFrames
  , aFrameLerp  :: Int  -- msec between frames
  , aIntialLerp :: Int  -- msec to get to first frame
  , aReversed   :: Bool -- true if animation is reversed
  , aFlipFlop   :: Bool -- true if animation should flipflop back to base
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
  { animationMap  :: Map AnimationType Animation
  , gender        :: Gender
  , footStep      :: FootStep
  , headOffset    :: V3 Float
  , fixedLegs     :: Bool
  , fixedTorso    :: Bool
  }
  deriving (Show, Eq, Ord)

animation = do
  first <- nat
  num <- int
  looping <- nat
  fps <- nat
  skip
  return $ Animation
    { aFirstFrame = first
    , aNumFrames  = if num < 0 then -num else num
    , aLoopFrames = looping
    , aFrameLerp  = round $ 1000 / max 1 (fromIntegral fps)
    , aIntialLerp = round $ 1000 / max 1 (fromIntegral fps)
    , aReversed   = num < 0
    , aFlipFlop   = False
    }

characterAttributes :: Parser (Character -> Character)
characterAttributes = fmap (\l x -> foldr ($) x l) $ many $ choice 
  [ (\a c -> c {footStep = a}) <$ kw "footsteps" <*> choice
      [ val FOOTSTEP_NORMAL "default"
      , val FOOTSTEP_NORMAL "normal"
      , val FOOTSTEP_BOOT "boot"
      , val FOOTSTEP_FLESH "flesh"
      , val FOOTSTEP_MECH "mech"
      , val FOOTSTEP_ENERGY "energy"
      ]
  , (\c -> c {fixedLegs = True}) <$ kw "fixedlegs"
  , (\c -> c {fixedTorso = True}) <$ kw "fixedtorso"
  , (\a c -> c {gender = a}) <$ kw "sex" <*> choice
      [ val GENDER_FEMALE "f"
      , val GENDER_NEUTER "n"
      , val GENDER_MALE "m"
      ]
  , (\x y z c -> c {headOffset = V3 x y z}) <$ kw "headoffset" <*> float <*> float <*> float
  ] <* skip


{-
  BOTH_DEATH1 - LEGS_TURN: must present
  TORSO_GETFLAG <= x <= TORSO_NEGATIVE: special setup if it is missing

  for each:
    numFrames < 0: reverse = True && numFrames *= -1
    frameLerp = 1000 / (max 1 fps)
    initialLerp = 1000 / (max 1 fps)

  postprocess:
    adjust first frame:
      skip = animations[LEGS_WALKCR].firstFrame - animations[TORSO_GESTURE].firstFrame
      LEGS_WALKCR <= x < TORSO_GETFLAG: firstFrame -= skip
    custom special setup:
      LEGS_BACKCR
      LEGS_BACKWALK
      FLAG_RUN
      FLAG_STAND
      FLAG_STAND2RUN
-}

character = do
  -- properties
  setAttribs <- characterAttributes
  -- animations
  anim <- Map.fromList <$> forM (enumFromTo BOTH_DEATH1 LEGS_TURN) (\a -> (a,) <$> animation)
  let torsoGesture = (anim ! TORSO_GESTURE) {aReversed = False, aFlipFlop = False}
      skip = aFirstFrame (anim ! LEGS_WALKCR) - aFirstFrame (anim ! TORSO_GESTURE)
      anim1 = foldr (Map.adjust (\a -> a {aFirstFrame = aFirstFrame a - skip})) anim (enumFromTo LEGS_WALKCR LEGS_TURN)
  -- unify the map
  animParsed <- (mappend anim1 . Map.fromList) <$> forM (enumFromTo TORSO_GETFLAG TORSO_NEGATIVE) (\a -> (a,) <$> (animation <|> pure torsoGesture))
  -- postprocess
  let animCalculated = Map.fromList
        [ (LEGS_BACKCR, (anim ! LEGS_WALKCR) {aReversed = True})
        , (LEGS_BACKWALK, (anim ! LEGS_WALK) {aReversed = True})
        , (FLAG_RUN, Animation
            { aFirstFrame = 0
            , aNumFrames  = 16
            , aLoopFrames = 16
            , aFrameLerp  = 1000 `div` 15
            , aIntialLerp = 1000 `div` 15
            , aReversed   = False
            , aFlipFlop   = False
            }
          )
        , (FLAG_STAND, Animation
            { aFirstFrame = 16
            , aNumFrames  = 5
            , aLoopFrames = 0
            , aFrameLerp  = 1000 `div` 20
            , aIntialLerp = 1000 `div` 20
            , aReversed   = False
            , aFlipFlop   = False
            }
          )
        , (FLAG_STAND2RUN, Animation
            { aFirstFrame = 16
            , aNumFrames  = 5
            , aLoopFrames = 1
            , aFrameLerp  = 1000 `div` 15
            , aIntialLerp = 1000 `div` 15
            , aReversed   = True
            , aFlipFlop   = False
            }
          )
        ]

  return . setAttribs $ Character
    { animationMap  = animParsed `mappend` animCalculated
    , gender        = GENDER_MALE
    , footStep      = FOOTSTEP_NORMAL
    , headOffset    = V3 0 0 0
    , fixedLegs     = False
    , fixedTorso    = False
    }

parseCharacter :: String -> ByteString -> Character
parseCharacter n s = eval n $ parse (skip *> character <* skip <* endOfInput) s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail leftover ctx err  -> error $ unlines ["fail:", "not consumed: " ++ BS.unpack leftover, "context: " ++ unwords ctx, "error: " ++ err]
        Partial f'  -> eval n (f' "")
