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
worldResources = concatMap resource . view wEntities where
  itemModels itemType = nub $ [R_MD3 model | i <- relatedItems itemType, model <- itWorldModel (itemMap ! i)]
  resource = \case
    EBullet b   -> fmap R_MD3 . maybeToList . wiMissileModel $ weaponInfoMap ! (b^.bType)
    EWeapon a   -> itemModels . IT_WEAPON $ a^.wType
    EAmmo a     -> itemModels . IT_AMMO $ a^.aType
    EArmor a    -> itemModels . IT_ARMOR $ a^.rType
    EHealth a   -> itemModels . IT_HEALTH $ a^.hType
    EHoldable h -> itemModels . IT_HOLDABLE $ h^.hoType
    EPowerup p  -> itemModels . IT_POWERUP $ p^.puType
    -- TODO:PLayer
    _           -> []

relatedItems :: ItemType -> [ItemType]
relatedItems i = [i]
