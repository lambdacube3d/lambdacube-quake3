{-# LANGUAGE FlexibleContexts #-}


module Monads where

import Entities
import Visuals
import World

import Control.Monad.RWS
import Control.Monad.Random
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe

import GameEngine.RenderSystem
import GameEngine.Collision
import GameEngine.Data.BSP
import GameEngine.Data.MD3
import GameEngine.RenderSystem

import System.Random.Mersenne.Pure64

import Data.Vect hiding (Vector)
import Data.Vect.Float.Instances
import Data.Vect.Float.Base hiding(Vector)
import Data.Vector (Vector,(!),(//))
import Data.Functor.Identity
import qualified Data.Vector as V


-- Entities have access to this data
data EntityEnvironment = EntityEnvironment 
 {
    resources :: ResourceCache
  , level     :: BSPLevel
  , userInput :: Input
  , gravity   :: Vec3
  , entites   :: Vector Entity
 }

type UpdateM w s = MaybeT (RWST EntityEnvironment w s (Rand PureMT))

type Collected_Objects = ([Entity], [Visual])

type EntityM e = UpdateM  Collected_Objects e

type VisualM v = UpdateM [Visual] v

type CollectMT m = WriterT Collected_Objects (RandT PureMT m)

type CollectM = CollectMT Identity

runUpdateM :: EntityEnvironment -> s -> UpdateM w s s -> PureMT -> (Maybe s, w)
runUpdateM entityEnvironment state update randGen = evalRand (evalRWST (runMaybeT update) entityEnvironment state) randGen

runEntityM :: EntityEnvironment -> e -> EntityM e e -> PureMT -> (Maybe e, ([Entity], [Visual]))
runEntityM = runUpdateM

runVisualM :: EntityEnvironment -> v -> VisualM v v -> PureMT -> (Maybe v, [Visual])
runVisualM = runUpdateM

runCollectMT :: Monad m => PureMT -> CollectMT m a -> m (([Entity], [Visual]), PureMT)
runCollectMT randGen collector = runRandT (execWriterT collector) randGen

runCollectM :: PureMT -> CollectM a -> (([Entity], [Visual]), PureMT)
runCollectM p c = runIdentity $ runCollectMT p c

addEntities :: MonadWriter ([Entity], [Visual]) m => [Entity] -> m ()
addEntities entities = tell (entities, [])

addEntity :: MonadWriter ([Entity], [Visual]) m => Entity -> m ()
addEntity = addEntities . pure

addVisuals :: MonadWriter ([Entity], [Visual]) m => [Visual] -> m ()
addVisuals visuals = tell ([], visuals)

addVisual :: MonadWriter ([Entity], [Visual]) m => Visual -> m ()
addVisual = addVisuals . pure

die :: MonadPlus m => m a
die = mzero