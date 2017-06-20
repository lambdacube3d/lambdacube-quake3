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
loadEntities = catMaybes . map loadEntity

itemMap :: Map String Item
itemMap = Map.fromList [(itClassName i,i) | i <- items]

loadEntity :: E.EntityData -> Maybe Entity
loadEntity E.EntityData{..} = case Map.lookup classname itemMap of
  Just Item{..} -> case itType of
    IT_HEALTH -> Just . EHealth $ Health
      { _hPosition  = origin
      , _hQuantity  = itQuantity
      }
    IT_WEAPON w -> Just . EWeapon $ Weapon
      { _wPosition  = origin
      , _wDropped   = False
      , _wType      = w
      }
    IT_AMMO w -> Just . EAmmo $ Ammo
      { _aPosition  = origin
      , _aQuantity  = itQuantity
      , _aDropped   = False
      , _aType      = w
      }
    IT_ARMOR -> Just . EArmor $ Armor
      { _rPosition  = origin
      , _rQuantity  = itQuantity
      , _rDropped   = False
      }
    IT_POWERUP p -> Just . EPowerup $ Powerup
      { _puPosition = origin
      , _puType     = p
      }
    IT_HOLDABLE h -> Just . EHoldable $ Holdable
      { _hoPosition = origin
      , _hoType     = h
      }
    _ -> Nothing
  Nothing -> case classname of
    "info_player_start"      -> spawnPoint
    "info_player_deathmatch" -> spawnPoint
    "team_CTF_bluespawn"     -> spawnPoint
    "team_CTF_redspawn"      -> spawnPoint
    "team_CTF_blueplayer"    -> spawnPoint
    "team_CTF_redplayer"     -> spawnPoint
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
  where
    spawnPoint = Just . ESpawnPoint $ SpawnPoint
      { _spPosition = origin
      , _spAngles   = angles
      }
