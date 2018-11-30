{-# LANGUAGE LambdaCase #-}
module LoadResources where

import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GameEngine.Scene (Resource(..))
import Lens.Micro.Platform
import World
import Items
import Entities

itemMap :: Map ItemType Item
itemMap = Map.fromList [(itType i,i) | i <- items]

weaponInfoMap :: Map Items.Weapon WeaponInfo
weaponInfoMap = Map.fromList [(wiType w,w) | w <- weaponInfos]

worldResources :: World -> [Resource]
worldResources = nub . concatMap resource . view wEntities where
  itemModels itemType
    | Just it <- Map.lookup itemType itemMap = [R_MD3 model | model <- itWorldModel it]
    | otherwise = []
  missileMD3 w = fmap R_MD3 . maybeToList $ do wiMissileModel =<< Map.lookup w weaponInfoMap
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

hudResources :: [Resource]
hudResources = (R_Shader . itIcon <$> items) ++ (R_Shader <$> Map.elems digitMap) where

type ShaderName = String

digitMap :: Map Char ShaderName
digitMap = Map.fromList
    [ ('0', "gfx/2d/numbers/zero_32b")
    , ('1', "gfx/2d/numbers/one_32b")
    , ('2', "gfx/2d/numbers/two_32b")
    , ('3', "gfx/2d/numbers/three_32b")
    , ('4', "gfx/2d/numbers/four_32b")
    , ('5', "gfx/2d/numbers/five_32b")
    , ('6', "gfx/2d/numbers/six_32b")
    , ('7', "gfx/2d/numbers/seven_32b")
    , ('8', "gfx/2d/numbers/eight_32b")
    , ('9', "gfx/2d/numbers/nine_32b")
    , ('-', "gfx/2d/numbers/minus_32b")
    ]
