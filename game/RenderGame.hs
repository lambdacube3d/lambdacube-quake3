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
import GameEngine.RenderSystem
import GameEngine.Utils

renderFun :: World -> Scene
renderFun w = Scene renderables (fromMaybe idmtx camera) where
  add l = tell (l,Last Nothing)
  setCamera c = tell ([],Last $ Just c)
  (renderables,Last camera) = execWriter . forM_ (w^.wEntities) $ \case
    EBullet b   -> add [MD3 (b^.bPosition) "models/ammo/rocket/rocket.md3"]
    EPlayer a   -> setCamera (cm .*. sm .*. pm) >> add [MD3 (a^.pPosition) "models/players/grunt/head.md3"] where
      cm = fromProjective (lookat camPos camTarget camUp)
      pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h)
      sm = fromProjective (scaling $ Vec3 s s s)
      s  = 0.005
      near = 0.00001/s
      far  = 100/s
      fovDeg = 60
      w = 800
      h = 600
      camPos = toVec3 $ a^.pPosition
      camTarget = camPos + toVec3 direction
      camUp = Vec3 0 0 1
      angle = a^.pAngle
      direction = unitVectorAtAngle $ degToRad angle
      toVec3 (Vec2 x y) = Vec3 x y 0
      unitVectorAtAngle = sinCos
      degToRad a = a/180*pi

    EWeapon a   -> add [MD3 (a^.wPosition) "models/weapons2/shotgun/shotgun.md3"]
    EAmmo a     -> add [MD3 (a^.aPosition) "models/powerups/ammo/shotgunam.md3"]
    EArmor a    -> add [MD3 (a^.rPosition) "models/powerups/armor/armor_red.md3"]
    EHealth a   -> add [MD3 pos "models/powerups/health/medium_cross.md3", MD3 pos "models/powerups/health/medium_sphere.md3"] where pos = a^.hPosition
    _ -> return ()
