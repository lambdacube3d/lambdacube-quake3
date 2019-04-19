{-# LANGUAGE LambdaCase, FlexibleContexts, RecordWildCards, OverloadedStrings #-}
module RenderGame where

import Text.Printf
import Data.Monoid
import Data.Maybe
import Control.Monad.Writer.Strict
import Data.Vect
import Data.Vect.Float.Instances
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Lens.Micro.Platform
import Entities
import World
import GameEngine.Scene
import GameEngine.Utils
import Debug.Trace
import Items
import LoadResources
import Debug.Trace


up = Vec3 0 0 1

cITEM_SCALEUP_TIME :: Float
cITEM_SCALEUP_TIME = 1.0

data RenderSettings
  = RenderSettings
  { windowWidth     :: !Int  -- local
  , windowHeight    :: !Int  -- local
  , sceneTime       :: !Float
  , mapFile         :: !String
  } deriving Show

add l = tell (l,Last Nothing, Last Nothing)
white = Vec4 1 1 1 1

attachedTo :: (MD3Data, Tag) -> MD3Data -> MD3Data
attachedTo (childData, tag) md3data = md3data{ md3Attachments = (tag, childData) : currentAttachments }
 where currentAttachments = md3Attachments md3data
  
renderFun :: RenderSettings -> WorldSnapshot -> Scene
renderFun RenderSettings{..} WorldSnapshot{..} =  Scene (BSPMap mapFile : renderables) pictures camera where
  setCamera c = tell ([],Last $ Just c, Last Nothing)
  setPlayer c = tell ([],Last Nothing, Last $ Just c)
  cam camPos camTarget = camera where
    camera = Camera
      { cameraPosition      = camPos
      , cameraOrientation   = fun camTarget up
      , cameraProjection    = (fromProjective $ quakeToGL .*. sm) .*. pm
      , cameraFrustum       = frust
      , cameraViewportSize  = (windowWidth,windowHeight)
      }
    quakeToGL = lookat zero (Vec3 1 0 0) up
    apsectRatio = fromIntegral windowWidth / fromIntegral windowHeight
    pm = perspective near far (fovDeg / 180 * pi) apsectRatio
    sm = scaling $ Vec3 s s s
    frust = frustum fovDeg apsectRatio near far camPos (camPos + camTarget) up
    s  = 0.005
    near = 0.00001/s
    far  = 100/s
    fovDeg = 70
    --EPlayer player = head $ filter isPlayer gameEntities
    --weaponMD5 = map (\path -> ) $ itWorldModel $ fromJust $ Map.lookup (IT_WEAPON (_pSelectedWeapon player)) itemMap 

  time' = sceneTime
  zaxis = Vec3 0 0 1
  rotation = rotU zaxis time'
  rotation' = rotU zaxis ((-1) * time')
  bobUpDown = 4 * cos ((sceneTime + 1000))
  bob = Vec3 0 0 bobUpDown

  pictures = fromMaybe [] statusBar

  yellow = Vec4 1 1 0 1

  statusBar = do
    player <- mplayer
    pure $ concat
      [ pure Picture
        { picturePosition = Vec2 0 0
        , pictureSize     = Vec2 100 100
        , pictureUV1      = Vec2 0 0
        , pictureUV2      = Vec2 1 1
        , pictureColor    = white
        , pictureShader   = itIcon (itemMap ! (IT_WEAPON $ player^.pSelectedWeapon))
        }
      , renderNum 120 30 yellow (player^.pHealth)
      , renderNum 270 30 white (player^.pArmor)
      , renderNum 370 30 white ((player^.pAmmos) ! (player^.pSelectedWeapon))
      ]

  ringCenterMod = Vec3 0 0 12
  camera = fromMaybe (cam (Vec3 0 0 0) (Vec3 1 0 0)) mcamera

  respawnScaleUp stime = if (time' - stime) < cITEM_SCALEUP_TIME
                            then (time' - stime) / cITEM_SCALEUP_TIME
                            else 1.0

  (renderables,Last mcamera,Last mplayer) = execWriter . forM_ gameEntities $ \case
    EPlayer a   -> setPlayer a >> setCamera (cam (a^.pPosition) (a^.pDirection)) >> renderPlayerWeapon itemMap weaponInfoMap time' a
	  
	where
    EBullet b   -- -> add [MD3 (b^.bPosition) one white "models/ammo/rocket/rocket.md3"]
                -> add [MD3 (b^.bPosition) (fun (b^.bDirection) (Vec3 0 0 1)) 1 white
                            (fromMaybe "models/ammo/rocket/rocket.md3"
                             . wiMissileModel
                             $ weaponInfoMap ! (b^.bType))]

    EWeapon a   -> add [ MD3New defaultMD3Data
                          { md3Position     = bob + (a^.wPosition)
                          , md3Orientation  = rotation
                          , md3Scale        = respawnScaleUp (a^.wTime)
                          , md3ModelFile    = model
                          }
                       | model <- itWorldModel (itemMap ! (IT_WEAPON $ a^.wType))
                       ]

    EAmmo a     -> add [MD3 (bob + (a^.aPosition)) rotation (respawnScaleUp (a^.aTime)) white model | model <- itWorldModel (itemMap ! (IT_AMMO $ a^.aType))]
    EArmor a    -> add [MD3 (bob + (a^.rPosition)) rotation (respawnScaleUp (a^.rTime)) white model | model <- itWorldModel (itemMap ! (IT_ARMOR $ a^.rType))]
    EHealth a   -> add [MD3 (bob + (a^.hPosition)) rotation (respawnScaleUp (a^.hTime)) white model | model <- itWorldModel (itemMap ! (IT_HEALTH $ a^.hType))]
    EHoldable a -> add [MD3 (bob + (a^.hoPosition)) rotation (respawnScaleUp (a^.hoTime)) white model | model <- itWorldModel (itemMap ! (IT_HOLDABLE $ a^.hoType))]
    EPowerup p  -> add $ do
      let respawnScale = respawnScaleUp (p^.puTime)
      case itWorldModel (itemMap ! (IT_POWERUP $ p^.puType)) of
        [model] -> [MD3 (bob + (p^.puPosition)) rotation respawnScale white model]
        [model1, model2] -> [ MD3 (bob + (p^.puPosition)) rotation respawnScale white model1
                            , MD3 (bob + (p^.puPosition) + ringCenterMod) rotation' respawnScale white model2
                            ]

    -- TEMP: just visualize targets
    --ETarget a   -> add [MD3Character (a^.ttPosition) one 1 white "visor" "default"]
    ETarget a   -> add [MD3New (playerModel "visor") {md3Position = a^.ttPosition}]
    _ -> return ()
    
    
    
renderPlayerWeapon :: MonadWriter ([Renderable], Last a1, Last a2) m => Map ItemType Item -> Map Items.Weapon WeaponInfo -> Float -> Player -> m ()
renderPlayerWeapon itemMap weaponInfoMap time p = let 
 frame = Just (min (floor $ 22 * (time - p^.pShootTime)) 5)
 weaponInfo = weaponInfoMap ! (p^.pSelectedWeapon)
 flashModelName = wiFlashModel weaponInfo
 flashModel = defaultMD3Data { md3ModelFile = fromJust flashModelName, md3Frame = frame }
 handModelName = fromJust $ wiHandModel weaponInfo  
 barrelModelName = wiBarrelModel weaponInfo
 handMD3 = defaultMD3Data
  { 
    md3Position = p^.pPosition,
    md3Orientation =  sphere_UV_toQuat $ p^.pRotationUV,
    md3ModelFile = handModelName,
    md3Frame = frame
  }
 attachWeaponToHand weaponModelName =  MD3New . attachBarrelModel  $ (setupWeapon weaponModelName, Tag "tag_weapon") `attachedTo` handMD3
 setupWeapon weaponModelName = attachFlashModel $ defaultMD3Data
  {
    md3ModelFile = weaponModelName,
    md3Frame = frame
  }
 attachFlashModel = if time - p^.pShootTime < 0.05 && isJust flashModelName then attachedTo (flashModel, Tag "tag_flash") else id  
 attachBarrelModel = maybe id (\modelname -> attachedTo (defaultMD3Data { md3ModelFile = modelname, md3Frame = frame }, Tag "tag_barrel")) barrelModelName
 in (add . map attachWeaponToHand . itWorldModel) (itemMap ! (IT_WEAPON $ p^.pSelectedWeapon))
  --(when (time - p^.pShootTime < 0.05 && isJust flashModel) $ add [MD3 (weaponPos + 6*(p^.pDirection)) weaponRotation 0.5 white $ fromJust flashModel])
	  
	

playerModel :: String -> MD3Data
playerModel name = defaultMD3Data
  { md3ModelFile    = printf "models/players/%s/lower.md3" name
  , md3Attachments  = [(Tag "tag_torso", torso)]
  , md3Frame        = Just 0
  , md3SkinName     = Just $ printf "models/players/%s/lower_default.skin" name
  } where
      torso = defaultMD3Data
        { md3ModelFile    = printf "models/players/%s/upper.md3" name
        , md3Attachments  = [(Tag "tag_head", head)]
        , md3Frame        = Just 0
        , md3SkinName     = Just $ printf "models/players/%s/upper_default.skin" name
        }
      head = defaultMD3Data
        { md3ModelFile  = printf "models/players/%s/head.md3" name
        , md3SkinName   = Just $ printf "models/players/%s/head_default.skin" name
        }

renderNum x y rgba value = concatMap digit $ zip [0..] $ printf "%d" value where
  digit (i,c) = case Map.lookup c digitMap of
    Nothing -> []
    Just shaderName ->
      [ Picture
        { picturePosition = Vec2 (x + 32 * i) y
        , pictureSize     = Vec2 32 48
        , pictureUV1      = Vec2 0 0
        , pictureUV2      = Vec2 1 1
        , pictureColor    = rgba
        , pictureShader   = shaderName
        }
      ]

fun direction desiredUp = rot2 .*. rot1 where
  rot1 = rotationBetweenVectors (Vec3 1 0 0) direction
  right = direction `crossprod` desiredUp
  desiredUp' = right `crossprod` direction
  newUp = rot1 *. Vec3 0 0 1
  rot2 = rotationBetweenVectors newUp desiredUp'

rotationBetweenVectors start_ dest_ = U $ Vec4 (s * 0.5) (x * invs) (y * invs) (z * invs) where
  start = normalize start_
  dest = normalize dest_
  cosTheta = start `dotprod` dest
  Vec3 x y z = start `crossprod` dest
  s = sqrt $ (1 + cosTheta) * 2
  invs = 1 / s

{-
quat RotationBetweenVectors(vec3 start, vec3 dest){
  start = normalize(start);
  dest = normalize(dest);

  float cosTheta = dot(start, dest);
  vec3 rotationAxis;

  if (cosTheta < -1 + 0.001f){
    // special case when vectors in opposite directions:
    // there is no "ideal" rotation axis
    // So guess one; any will do as long as it's perpendicular to start
    rotationAxis = cross(vec3(0.0f, 0.0f, 1.0f), start);
    if (gtx::norm::length2(rotationAxis) < 0.01 ) // bad luck, they were parallel, try again!
      rotationAxis = cross(vec3(1.0f, 0.0f, 0.0f), start);

    rotationAxis = normalize(rotationAxis);
    return gtx::quaternion::angleAxis(180.0f, rotationAxis);
  }

  rotationAxis = cross(start, dest);

  float s = sqrt( (1+cosTheta)*2 );
  float invs = 1 / s;

  return quat(
    s * 0.5f, 
    rotationAxis.x * invs,
    rotationAxis.y * invs,
    rotationAxis.z * invs
  );

}
-}