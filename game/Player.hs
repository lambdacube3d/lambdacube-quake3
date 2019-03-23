{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, NoMonomorphismRestriction, FlexibleContexts #-}
module Player where

import Entities
import World

import Control.Monad
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Vect hiding (Vector)
import Lens.Micro.Platform


addEntities ents = tell (ents,[])

shoots Input{..} = do
  shootTime <- view pShootTime
  when (shoot && time - shootTime > 0.1) $ do
    weapon  <- use pSelectedWeapon
    hasAmmo <- (maybe False (>0) . Map.lookup weapon) <$> use pAmmos
    when hasAmmo $ do
      pAmmos %= Map.update (\amount -> Just $ if amount < 1 then 0 else (amount - 1))
                           weapon
      pos       <- use pPosition
      direction <- use pDirection
      addEntities [EBullet $ Bullet (pos + 50 *& direction) (500 *& direction) 1 2 weapon]
      pShootTime .= time

changeWeapon Input{..} | isNothing changeWeapon = pure ()
changeWeapon Input{..} = do
  let Just newWeapon = changeWeapon
  weapons <- use pWeapons
  when (newWeapon `Set.member` weapons) $ do
    pSelectedWeapon .= newWeapon

togglesHoldable Input{..} | isNothing toggleHoldable = pure ()
togglesHoldable Input{..} = do
  let Just holdable = toggleHoldable
  holdables <- use pHoldables
  when (holdable `Map.member` holdables) $ do
    pHoldables %= Map.update (\(active, time) -> Just (not active, time)) holdable

tickHoldables Input{..} = do
  pHoldables %= Map.mapMaybe (\(active, rest) ->
    case (active, rest - dtime < 0) of
      (True, True)  -> Nothing
      (True, False) -> Just (active, rest - dtime)
      (False, _)    -> Just (active, rest))
