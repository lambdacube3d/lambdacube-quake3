{-# LANGUAGE RecordWildCards #-}
module LoadEntities
  ( loadEntities
  ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vect
import Data.Vect.Float.Util.Quaternion

import Entities
import Items
import qualified GameEngine.Loader.Entity as E

loadEntities :: [E.EntityData] -> [Entity]
loadEntities = (player0:) . catMaybes . map loadEntity where
  player0 = EPlayer $ Player
    { _pPosition    = Vec3 0 0 0
    , _pDirection   = Vec3 1 0 0
    , _pFVelocity   = 0
    , _pSVelocity   = 0
    , _pHealth      = 100
    , _pAmmo        = 10
    , _pArmor       = 0
    , _pShootTime   = 0
    , _pDamageTimer = 0
    , _pName        = "grunt"
    , _pId          = 0
    }


itemMap :: Map String Item
itemMap = Map.fromList [(itClassName i,i) | i <- items]

loadEntity :: E.EntityData -> Maybe Entity
loadEntity E.EntityData{..} = case Map.lookup classname itemMap of
  Just Item{..} -> case itType of
    IT_HEALTH -> Just . EHealth $ Health
      { _hPosition  = origin
      , _hQuantity  = itQuantity
      }
    IT_WEAPON _ -> Just . EWeapon $ Weapon
      { _wPosition  = origin
      , _wDropped   = False
      }
    IT_AMMO _ -> Just . EAmmo $ Ammo
      { _aPosition  = origin
      , _aQuantity  = itQuantity
      , _aDropped   = False
      }
    IT_ARMOR -> Just . EArmor $ Armor
      { _rPosition  = origin
      , _rQuantity  = itQuantity
      , _rDropped   = False
      }
    _ -> Nothing
  Nothing -> case classname of
    "trigger_teleport" -> do
      target_ <- target
      Just . ETeleport $ Teleport
        { _tPosition  = origin
        , _tTarget    = target_
        }
    "trigger_push" -> do 
      target_ <- target
      Just . ETeleport $ Teleport -- HACK
        { _tPosition  = origin
        , _tTarget    = target_
        }
    _ | classname `elem` ["target_position","misc_teleporter_dest","info_notnull"] -> do
      targetname_ <- targetname
      Just . ETarget $ Target
        { _ttPosition   = origin
        , _ttTargetName = targetname_
        }
    _ -> Nothing
