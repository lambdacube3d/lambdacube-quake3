{-# language LambdaCase, ViewPatterns #-}
module GameEngine.Graphics.GameCharacter where

import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString,unpack)
import Text.Printf
import GameEngine.Data.GameCharacter
import GameEngine.Graphics.Render
import GameEngine.Loader.MD3
import GameEngine.Loader.GameCharacter
import GameEngine.Loader.Zip
import LambdaCube.GL

{-
  character:
    skin: default, blue, red  -- SKIN
    name: anarki
    resources:
      md3 models: head, upper, lower -- BODYPART
      skin: BODYPART_SKIN.skin
      animation: animation.cfg

    loaded resources:
      - we can not reuse/share geometry data because we mutate it's content with current animation frame
        FIXME: improve lambdacube-gl API to support update object's stream input
      GPUCharacter -- not possible yet
        Character
        skinMap :: Map String String
        head    :: GPUMD3
        upper   :: GPUMD3
        lower   :: GPUMD3

      CharacterInstance
        Character
        head  :: MD3Instance
        upper :: MD3Instance
        lower :: MD3Instance

      setupCharacterInstance
        set animation frames
        snap part: lower, upper, head
        set body part rotation: upper, head
        optional: snap weapon to hand
-}

data CharacterInstance
  = CharacterInstance
  { characterinstanceCharacter  :: Character
  , characterinstanceHeadModel  :: MD3Instance
  , characterinstanceUpperModel :: MD3Instance
  , characterinstanceLowerModel :: MD3Instance
  }

readSkin :: ByteString -> Map String String
readSkin txt = Map.fromList
  [ (head k,head v)
  | l <- lines $ unpack txt
  , i <- maybeToList $ elemIndex ',' l
  , let (words . map toLower -> k,words . map toLower . tail -> v) = splitAt i l
  , not . null $ k
  , not . null $ v
  ]

addCharacterInstance :: Map String Entry -> GLStorage -> String -> String -> IO CharacterInstance
addCharacterInstance pk3 storage name skin = do
  let skinName part   = printf "models/players/%s/%s_%s.skin" name part skin
      modelName part  = printf "models/players/%s/%s.md3" name part
      animationName   = printf "models/players/%s/animation.cfg" name
      getEntry n = readEntry =<< case Map.lookup n pk3 of
        Nothing -> fail $ printf "file not found: %s" n
        Just a  -> return a
      loadInstance part = do
        model <- readMD3 . LB.fromStrict <$> getEntry (modelName part)
        skin <- readSkin <$> getEntry (skinName part)
        addMD3 storage model skin ["worldMat"]

  character <- parseCharacter animationName <$> getEntry animationName >>= \case
    Left message  -> fail message
    Right a -> return a

  headInstance <- loadInstance "head"
  upperInstance <- loadInstance "upper"
  lowerInstance <- loadInstance "lower"

  return $ CharacterInstance
    { characterinstanceCharacter  = character
    , characterinstanceHeadModel  = headInstance
    , characterinstanceUpperModel = upperInstance
    , characterinstanceLowerModel = lowerInstance
    }
