{-# LANGUAGE LambdaCase #-}
module RenderGame where

import Data.Vect
import Lens.Micro.Platform
import GameEngine.Logic.Entities
import GameEngine.Logic.World

data Renderable
  = MD3 Vec2 String -- model
  deriving Show

renderFun :: World -> [Renderable]
renderFun w = ents where
  ents = flip concatMap (w^.wEntities) $ \case
    EBullet b   -> [MD3 (b^.bPosition) "models/ammo/rocket/rocket.md3"]
    EPlayer a   -> [MD3 (a^.pPosition) "models/players/grunt/head.md3"]
    EWeapon a   -> [MD3 (a^.wPosition) "models/weapons2/shotgun/shotgun.md3"]
    EAmmo a     -> [MD3 (a^.aPosition) "models/powerups/ammo/shotgunam.md3"]
    EArmor a    -> [MD3 (a^.rPosition) "models/powerups/armor/armor_red.md3"]
    EHealth a   -> [MD3 pos "models/powerups/health/medium_cross.md3", MD3 pos "models/powerups/health/medium_sphere.md3"] where pos = a^.hPosition
    _ -> []
