{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, ViewPatterns #-}
module GameEngine.Entity where

import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import Data.Attoparsec.ByteString.Char8
import Data.Vect
import Data.Maybe

import GameEngine.Loader.ShaderParser
import GameEngine.Data.Items

data Entity
  = TriggerTeleport     SB.ByteString Int
  | TargetPosition      SB.ByteString Vec3
--  | MiscTeleporterDest  String Float Vec3
--  | InfoPlayerDeathMatch !Float !Vec3
--  | ItemEntity Item
  deriving Show

loadTeleports :: [T.Trie SB.ByteString] -> ([Entity],T.Trie Entity)
loadTeleports t =
  (
  [ TriggerTeleport target (read model)
  | e <- t
  , className <- maybeToList $ T.lookup "classname" e
  , className `elem` ["trigger_teleport","trigger_push"] -- HACK
  , target <- maybeToList $ T.lookup "target" e
  , (SB.unpack -> '*':model) <- maybeToList $ T.lookup "model" e
  ]
  ,
  T.fromList
  [ (targetName,TargetPosition targetName (Vec3 x y z))
  | e <- t
  , className <- maybeToList $ T.lookup "classname" e
  , className `elem` ["target_position","misc_teleporter_dest","info_notnull"]
  , targetName <- maybeToList $ T.lookup "targetname" e
  , origin <- maybeToList $ T.lookup "origin" e
  , let [x,y,z] = map read $ words $ SB.unpack origin
  ]
  )

parseEntities :: String -> SB.ByteString -> [T.Trie SB.ByteString]
parseEntities n s = eval n $ parse entities s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")

{-
    let ents = parseEntities bspName $ blEntities bsp
        spawnPoint e
          | Just classname <- T.lookup "classname" e
          , classname `elem` ["info_player_deathmatch"]
          , Just origin <- T.lookup "origin" e
          , [x,y,z] <- map read $ words $ SB.unpack origin = [Vec3 x y z]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints
-}