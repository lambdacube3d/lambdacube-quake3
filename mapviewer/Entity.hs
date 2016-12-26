{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, ViewPatterns #-}
module Entity where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vect
import Data.Maybe

import Items
import GameEngine.Loader.Entity

data Entity
  = TriggerTeleport     SB.ByteString Int
  | TargetPosition      SB.ByteString Vec3
--  | MiscTeleporterDest  String Float Vec3
--  | InfoPlayerDeathMatch !Float !Vec3
--  | ItemEntity Item
  deriving Show

loadTeleports :: [EntityData] -> ([Entity],Map ByteString Entity)
loadTeleports t = (teleports,targets) where
  teleports =
    [ TriggerTeleport (SB.pack target_) (read model_)
    | EntityData{..} <- t
    , classname `elem` ["trigger_teleport","trigger_push"] -- HACK
    , target_ <- maybeToList target
    , ('*':model_) <- maybeToList model
    ]
  targets = Map.fromList
    [ (targetName_,TargetPosition targetName_ origin)
    | EntityData{..} <- t
    , classname `elem` ["target_position","misc_teleporter_dest","info_notnull"]
    , targetName <- maybeToList targetname
    , let targetName_ = SB.pack targetName
    ]

{-
    let ents = parseEntities bspName $ blEntities bsp
        spawnPoint e
          | Just classname <- Map.lookup "classname" e
          , classname `elem` ["info_player_deathmatch"]
          , Just origin <- Map.lookup "origin" e
          , [x,y,z] <- map read $ words $ SB.unpack origin = [Vec3 x y z]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints
-}