{-# LANGUAGE LambdaCase #-}
module Collision where

import Data.Maybe
import Data.List
import Data.Vect
import Lens.Micro.Platform
--import qualified Data.KdMap.Static as KDT
import GameEngine.RenderSystem
import Entities

getCollisions :: RenderSystem -> [Entity] -> [(Int,Int)]
getCollisions engine entities = result where
  radius = 40
  distanceSquare = radius ^ 2
  result = [(aId,bId) | (aId,a):ents <- tails $ zip [0..] entities, (bId,b) <- ents, collide a b]
  collide a b = fromMaybe False $ do
    p1 <- getPosition a
    p2 <- getPosition b
    return $ normsqr (p1 &- p2) <= distanceSquare
  {-
    - build kd-tree with collision shape and entity id
    - query entities from kd-tree
  -}

  getPosition :: Entity -> Maybe Vec3
  getPosition = \case
    EPlayer a   -> Just (a^.pPosition)
    EBullet b   -> Just (b^.bPosition) -- "models/ammo/rocket/rocket.md3"
    EWeapon a   -> Just (a^.wPosition) -- "models/weapons2/shotgun/shotgun.md3"
    EAmmo a     -> Just (a^.aPosition) -- "models/powerups/ammo/shotgunam.md3"
    EArmor a    -> Just (a^.rPosition) -- "models/powerups/armor/armor_red.md3"
    EHealth a   -> Just (a^.hPosition) -- "models/powerups/health/medium_cross.md3"
    _ -> Nothing
