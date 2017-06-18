{-# LANGUAGE LambdaCase, FlexibleContexts, RecordWildCards #-}
module RenderGame where

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

renderFun :: World -> Scene
renderFun w = Scene (BSPMap (w^.wMapFile) : renderables) camera where
  add l = tell (l,Last Nothing)
  setCamera c = tell ([],Last $ Just c)
  white = Vec4 1 1 1 1
  cam camPos camTarget = camera where
    Input{..} = w^.wInput
    camera = Camera
      { cameraPosition      = camPos
      , cameraOrientation   = fun camTarget up
      , cameraProjection    = (fromProjective $ quakeToGL .*. sm) .*. pm
      , cameraFrustum       = frust
      , cameraViewportSize  = (windowWidth,windowHeight)
      }
    up = Vec3 0 0 1
    quakeToGL = lookat zero (Vec3 1 0 0) up
    apsectRatio = fromIntegral windowWidth / fromIntegral windowHeight
    pm = perspective near far (fovDeg / 180 * pi) apsectRatio
    sm = scaling $ Vec3 s s s
    frust = frustum fovDeg apsectRatio near far camPos (camPos + camTarget) up
    s  = 0.005
    near = 0.00001/s
    far  = 100/s
    fovDeg = 60

  camera = fromMaybe (cam (Vec3 0 0 0) (Vec3 1 0 0)) mcamera 
  (renderables,Last mcamera) = execWriter . forM_ (w^.wEntities) $ \case
    EPlayer a   -> setCamera $ cam (a^.pPosition) (a^.pDirection) {- >> add [MD3 (a^.pPosition) "models/players/grunt/head.md3"]-} where
    EBullet b   -> add [MD3 (b^.bPosition) one white "models/ammo/rocket/rocket.md3"]
    EWeapon a   -> add [MD3 (a^.wPosition) one white model | model <- itWorldModel (itemMap ! (IT_WEAPON $ a^.wType))]
    EAmmo a     -> add [MD3 (a^.aPosition) one white model | model <- itWorldModel (itemMap ! (IT_AMMO $ a^.aType))]
    EArmor a    -> add [MD3 (a^.rPosition) one white "models/powerups/armor/armor_red.md3"]
    EHealth a   -> add [MD3 pos one white "models/powerups/health/medium_cross.md3"
                       ,MD3 pos one white "models/powerups/health/medium_sphere.md3"] where pos = a^.hPosition

    -- TEMP: just visualize targets
    ETarget a   -> add [MD3Character (a^.ttPosition) one white "visor" "default"]
    _ -> return ()

itemMap :: Map ItemType Item
itemMap = Map.fromList [(itType i,i) | i <- items]

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