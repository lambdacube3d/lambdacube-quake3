{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module RenderGame where

import Data.Monoid
import Data.Maybe
import Control.Monad.Writer.Strict
import Data.Vect
import Data.Vect.Float.Instances
import Lens.Micro.Platform
import Entities
import World
import GameEngine.Scene
import GameEngine.Utils

renderFun :: World -> Scene
renderFun w = Scene (BSPMap (w^.wMapFile) : renderables) camera where
  add l = tell (l,Last Nothing)
  setCamera c = tell ([],Last $ Just c)
  white = Vec4 1 1 1 1
  (renderables,Last (Just camera)) = execWriter . forM_ (w^.wEntities) $ \case
    EPlayer a   -> setCamera camera {- >> add [MD3 (a^.pPosition) "models/players/grunt/head.md3"]-} where
      camera = Camera
        { cameraPosition      = camPos
        , cameraOrientation   = rotU up $ degToRad angle
        , cameraProjection    = (fromProjective $ quakeToGL .*. sm) .*. pm
        , cameraFrustum       = frust
        , cameraViewportSize  = (w,h)
        }
      up = Vec3 0 0 1
      quakeToGL = lookat zero (Vec3 1 0 0) up
      pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h)
      sm = scaling $ Vec3 s s s
      frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos (camPos + camTarget) up
      s  = 0.005
      near = 0.00001/s
      far  = 100/s
      fovDeg = 60
      w = 800
      h = 600
      camPos = toVec3 $ a^.pPosition
      camTarget = toVec3 direction
      angle = a^.pAngle
      direction = unitVectorAtAngle $ degToRad angle
      toVec3 (Vec2 x y) = Vec3 x y 0
      unitVectorAtAngle = sinCos
      degToRad a = a/180*pi

    EBullet b   -> add [MD3 (extendZero $ b^.bPosition) one white "models/ammo/rocket/rocket.md3"]
    EWeapon a   -> add [MD3 (extendZero $ a^.wPosition) one white "models/weapons2/shotgun/shotgun.md3"]
    EAmmo a     -> add [MD3 (extendZero $ a^.aPosition) one white "models/powerups/ammo/shotgunam.md3"]
    EArmor a    -> add [MD3 (extendZero $ a^.rPosition) one white "models/powerups/armor/armor_red.md3"]
    EHealth a   -> add [MD3 pos one white "models/powerups/health/medium_cross.md3"
                       ,MD3 pos one white "models/powerups/health/medium_sphere.md3"] where pos = extendZero $ a^.hPosition
    ETarget a   -> add [MD3Character (extendZero $ a^.ttPosition) one white "visor" "default"]
    _ -> return ()
