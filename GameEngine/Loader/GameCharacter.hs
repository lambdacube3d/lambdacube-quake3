{-# LANGUAGE OverloadedStrings, TupleSections #-}
module GameEngine.Loader.GameCharacter
  ( parseCharacter
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.HashMap.Strict (HashMap,(!))
import qualified Data.HashMap.Strict as HashMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import Text.Megaparsec hiding (count)
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L
import LambdaCube.Linear (V3(..))

import GameEngine.Data.GameCharacter

parseCharacter :: String -> ByteString -> Either String Character
parseCharacter fname src = case parse (spaceConsumer *> character <* eof) fname $ SB8.map toLower src of
  Left err  -> Left (parseErrorPretty err)
  Right e   -> Right e

animation :: Parser Animation
animation = do
  first <- integer
  num <- signedInteger
  looping <- integer
  fps <- integer
  spaceConsumer
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
  [ (\a c -> c {footStep = a}) <$ symbol "footsteps" <*> choice
      [ value FOOTSTEP_NORMAL "default"
      , value FOOTSTEP_NORMAL "normal"
      , value FOOTSTEP_BOOT "boot"
      , value FOOTSTEP_FLESH "flesh"
      , value FOOTSTEP_MECH "mech"
      , value FOOTSTEP_ENERGY "energy"
      ]
  , (\c -> c {fixedLegs = True}) <$ symbol "fixedlegs"
  , (\c -> c {fixedTorso = True}) <$ symbol "fixedtorso"
  , (\a c -> c {gender = a}) <$ symbol "sex" <*> choice
      [ value GENDER_FEMALE "f"
      , value GENDER_NEUTER "n"
      , value GENDER_MALE "m"
      ]
  , (\x y z c -> c {headOffset = V3 x y z}) <$ symbol "headoffset" <*> signedFloat <*> signedFloat <*> signedFloat
  ] <* spaceConsumer


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

character :: Parser Character
character = do
  -- properties
  setAttribs <- characterAttributes
  -- animations
  anim <- HashMap.fromList <$> forM (enumFromTo BOTH_DEATH1 LEGS_TURN) (\a -> (a,) <$> animation)
  let torsoGesture = (anim ! TORSO_GESTURE) {aReversed = False, aFlipFlop = False}
      skip = aFirstFrame (anim ! LEGS_WALKCR) - aFirstFrame (anim ! TORSO_GESTURE)
      anim1 = foldr (HashMap.adjust (\a -> a {aFirstFrame = aFirstFrame a - skip})) anim (enumFromTo LEGS_WALKCR LEGS_TURN)
  -- unify the map
  animParsed <- (mappend anim1 . HashMap.fromList) <$> forM (enumFromTo TORSO_GETFLAG TORSO_NEGATIVE) (\a -> (a,) <$> (animation <|> pure torsoGesture))
  -- postprocess
  let animCalculated = HashMap.fromList
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

-- parser primitives
lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

integer :: Parser Int
integer = fromIntegral <$> lexeme L.integer

signedInteger :: Parser Int
signedInteger = L.signed spaceConsumer integer

signedFloat :: Parser Float
signedFloat = realToFrac <$> L.signed spaceConsumer (lexeme float) where
  float = choice
    [ try L.float
    , try ((read . ("0."++)) <$ char '.' <*> some digitChar)
    , fromIntegral <$> L.integer
    ]

value :: a -> String -> Parser a
value v w = const v <$> symbol w
