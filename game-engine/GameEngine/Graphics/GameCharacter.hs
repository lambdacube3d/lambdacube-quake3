{-# language LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings #-}
module GameEngine.Graphics.GameCharacter where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.ByteString.Char8 (ByteString,unpack)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import Text.Printf
import Data.Vect
import Data.Vect.Float.Util.Quaternion

import LambdaCube.GL
import LambdaCube.Linear

import GameEngine.Data.GameCharacter
import GameEngine.Data.MD3
import GameEngine.Loader.MD3
import GameEngine.Loader.GameCharacter
import GameEngine.Loader.Zip
import GameEngine.Graphics.MD3
import GameEngine.Graphics.Frustum
import GameEngine.Utils

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

addCharacterInstance :: Map String Entry -> GLStorage -> String -> String -> IO CharacterInstance
addCharacterInstance pk3 storage name skin = do
  let skinName part   = printf "models/players/%s/%s_%s.skin" name part skin
      modelName part  = printf "models/players/%s/%s.md3" name part
      animationName   = printf "models/players/%s/animation.cfg" name
      getEntry n = readEntry =<< case Map.lookup n pk3 of
        Nothing -> fail $ printf "file not found: %s" n
        Just a  -> return a
      loadInstance :: String -> IO MD3Instance
      loadInstance part = do
        model <- readMD3 . LB.fromStrict <$> getEntry (modelName part)
        skin <- readMD3Skin <$> getEntry (skinName part)
        addMD3 storage model skin ["worldMat","entityRGB","entityAlpha"]

  character <- parseCharacter animationName . unpack <$> getEntry animationName >>= \case
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

sampleCharacterAnimation = V.fromList $
  [ (TORSO_GESTURE,LEGS_IDLE)
  , (TORSO_ATTACK,LEGS_IDLE)
  , (TORSO_ATTACK2,LEGS_IDLE)
  , (TORSO_DROP,LEGS_IDLE)
  , (TORSO_RAISE,LEGS_IDLE)
  , (TORSO_STAND,LEGS_IDLE)
  , (TORSO_STAND2,LEGS_IDLE)
  , (TORSO_GETFLAG,LEGS_IDLE)
  , (TORSO_GUARDBASE,LEGS_IDLE)
  , (TORSO_PATROL,LEGS_IDLE)
  , (TORSO_FOLLOWME,LEGS_IDLE)
  , (TORSO_AFFIRMATIVE,LEGS_IDLE)
  , (TORSO_NEGATIVE,LEGS_IDLE)

  , (TORSO_STAND,LEGS_WALKCR)
  , (TORSO_STAND,LEGS_WALK)
  , (TORSO_STAND,LEGS_RUN)
  , (TORSO_STAND,LEGS_BACK)
  , (TORSO_STAND,LEGS_SWIM)

  , (TORSO_STAND,LEGS_JUMP)
  , (TORSO_STAND,LEGS_LAND)

  , (TORSO_STAND,LEGS_JUMPB)
  , (TORSO_STAND,LEGS_LANDB)

  , (TORSO_STAND,LEGS_IDLE)
  , (TORSO_STAND,LEGS_IDLECR)

  , (TORSO_STAND,LEGS_TURN)


  , (TORSO_STAND,LEGS_BACKCR)
  , (TORSO_STAND,LEGS_BACKWALK)
  ] ++ zip bothAnim bothAnim where bothAnim = [BOTH_DEATH1, BOTH_DEAD1, BOTH_DEATH2, BOTH_DEAD2, BOTH_DEATH3, BOTH_DEAD3]

-- TODO: design proper interface
setupGameCharacter :: CharacterInstance -> Float -> Frustum -> Vec3 -> UnitQuaternion -> Float -> Vec4 -> IO ()
setupGameCharacter CharacterInstance{..} time cameraFrustum position orientation scale rgba = do
  let t100 = floor $ time / 4
      (torsoAnimType,legAnimType) = sampleCharacterAnimation V.! (t100 `mod` V.length sampleCharacterAnimation)

      -- torso = upper
      --  transform torso to legs
      --  transform head to torso (and legs)
      t = floor $ time * 15
      Character{..} = characterinstanceCharacter
      legAnim       = animationMap HashMap.! legAnimType
      legFrame      = aFirstFrame legAnim + t `mod` aNumFrames legAnim
      torsoAnim     = animationMap HashMap.! torsoAnimType
      torsoFrame    = aFirstFrame torsoAnim + t `mod` aNumFrames torsoAnim

      rgb       = trim rgba
      alpha     = _4 rgba
      worldMat  = toWorldMatrix position orientation scale

      lcMat :: Proj4 -> M44F
      lcMat m = mat4ToM44F . fromProjective $ m .*. rotationEuler (Vec3 (time/5) 0 0) .*. worldMat

      tagToProj4 :: Tag -> Proj4
      tagToProj4 Tag{..} = translateAfter4 tgOrigin (orthogonal . toOrthoUnsafe $ tgRotationMat)

      getTagProj4 :: MD3Instance -> Int -> ByteString -> Proj4
      getTagProj4 MD3Instance{..} frame name = case mdTags md3instanceModel V.!? frame >>= HashMap.lookup name of
        Nothing   -> idmtx
        Just tag  -> tagToProj4 tag

      lowerMat = one :: Proj4
      upperMat = getTagProj4 characterinstanceLowerModel legFrame "tag_torso"
      headMat  = getTagProj4 characterinstanceUpperModel torsoFrame "tag_head" .*. upperMat

      --p = trim . _4 $ fromProjective mat
      setup m obj = do
        --let finalMat = mat4ToM44F . fromProjective $ getTagProj4 characterinstanceUpperModel torsoFrame "tag_head" .*. getTagProj4 characterinstanceLowerModel legFrame "tag_torso" .*. one .*. worldMat
        uniformM44F "worldMat" (objectUniformSetter obj) $ lcMat m
        uniformV3F "entityRGB" (objectUniformSetter obj) $ vec3ToV3F rgb
        uniformFloat "entityAlpha" (objectUniformSetter obj) alpha
        enableObject obj $ pointInFrustum position cameraFrustum
        --cullObject obj p

  -- snap body parts
  forM_ (md3instanceObject characterinstanceHeadModel)  $ setup headMat
  forM_ (md3instanceObject characterinstanceUpperModel) $ setup upperMat
  forM_ (md3instanceObject characterinstanceLowerModel) $ setup lowerMat

  -- set animation frame geometry
  --setMD3Frame hLC frame
  setMD3Frame characterinstanceUpperModel torsoFrame
  setMD3Frame characterinstanceLowerModel legFrame
