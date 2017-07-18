{-# LANGUAGE LambdaCase #-}
module LoadResources where

import Data.List
import Data.Maybe
import Data.Map.Strict
import GameEngine.Scene (Resource(..))
import Lens.Micro.Platform
import World
import Items
import RenderGame
import Entities


worldResources :: World -> [Resource]
worldResources = nub . concatMap resource . view wEntities where
  itemModels itemType = [R_MD3 model | model <- itWorldModel (itemMap ! itemType)]
  missileMD3 w = fmap R_MD3 . maybeToList . wiMissileModel $ weaponInfoMap ! w
  resource = \case
    EBullet b   -> missileMD3 (b^.bType)
    EWeapon a   -> missileMD3 (a^.wType) ++ (itemModels . IT_WEAPON $ a^.wType)
    EAmmo a     -> itemModels . IT_AMMO $ a^.aType
    EArmor a    -> itemModels . IT_ARMOR $ a^.rType
    EHealth a   -> itemModels . IT_HEALTH $ a^.hType
    EHoldable h -> itemModels . IT_HOLDABLE $ h^.hoType
    EPowerup p  -> itemModels . IT_POWERUP $ p^.puType
    -- TODO:PLayer
    _           -> []
