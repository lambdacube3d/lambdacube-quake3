{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, NoMonomorphismRestriction, FlexibleContexts #-}
module GameLogic where 

import Control.Monad
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import System.Random.Mersenne.Pure64
import Lens.Micro.Platform
import Data.Map.Strict as Map (Map, lookup)
import Data.Vect hiding (Vector)
import Data.Vect.Float.Instances
import Data.Vect.Float.Base hiding(Vector)
import qualified Data.Vect.Float.Base as VB (_1, _2, _3)
import Data.Vect.Float.Util.Quaternion


import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Control.Monad.ST
import Data.Functor.Identity
import Control.Monad.Random
import System.Random.Mersenne.Pure64

import Data.Vector (Vector,(!),(//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import GameEngine.RenderSystem
import GameEngine.Collision
import GameEngine.Data.BSP
import GameEngine.Data.MD3
import GameEngine.RenderSystem

import qualified Items
import Entities
import Visuals
import World
import Collision
import qualified Player
import Monads
import Movers

import Debug.Trace


type Time = Float
type DTime = Float

-- visual item update monad + collect new visual items
type VM s a = ReaderT s (StateT s (MaybeT (WriterT ([Visual]) (Rand PureMT)))) a

-- monad for collect new entites or visuals
type CM a = WriterT ([Entity],[Visual]) (Rand PureMT) a


update :: Monad f => (s -> e) -> s -> ReaderT s (StateT s (MaybeT f)) a2 -> f (Maybe e)
update f a m = fmap f <$> runMaybeT (execStateT (runReaderT m a) a)

collect :: Monoid w => PureMT -> WriterT w (Rand PureMT) a -> ((a,w),PureMT)
collect randGen m = runIdentity $ runRandT (runWriterT m) randGen
  
respawn :: Time -> (e -> Entity) -> EntityM e ()
respawn time create_entity = do
  entity <- get
  t' <- getRandomR (time + 5, time + 10)
  addEntities [PSpawn $ Spawn t' $ create_entity entity]
  die

data Interaction
  = PlayerTeleportKillboxPlayers Player Teleport Killbox [Player]
  -- player item
  | PlayerHealth  Player Health
  | PlayerArmor   Player Armor
  | PlayerAmmo    Player Ammo
  | PlayerWeapon  Player Weapon
  -- player level
  | PlayerLava    Player Lava
  -- bullet
  | PlayerBullet  Player Bullet
  -- add: bullet-level,bullet-item

{-
  collect collided entities
  transform entities in interaction, collect newly generated entities
  step entities, also collect generated entities
  append generated entities

updateEntities :: BSPLevel -> ResourceCache -> RenderSystem -> PureMT -> Input -> [Entity] -> (PureMT,[Entity],[Visual])
updateEntities level resources engine randGen input@Input{..} ents = (randGen',catMaybes (V.toList nextEnts) ++ newEnts,newVisuals) where

  entityVector :: Vector (Maybe Entity)
  entityVector = V.fromList $ map Just ents

  collisions :: [(Int,Int)] -- post process collisions into interaction events
  collisions = getCollisions engine ents

  interactions :: [(Int,Int)] -> [Interaction]
  interactions _ = [] -- TODO
    {-
      the problem is that potentially we can add any kind of interaction rules, where anything can be interact with other objects
      in case of teleport and killbox the teleport is entangled with the teleport-target/killbox in position
      DESIGN CHOICES:
        #1 - preprocess and cache collisions, then construct interactions, where entangled items can refer to each other (references/ids)
        #2 - collision is checked during the interaction checking
      HINT:
        teleport is also connected to killbox and target
    -}

  -- step game and get next state
  ((nextEnts,(newEnts,newVisuals)),randGen') = collect randGen $ do
    let go entV (i1,i2) = case (entV ! i1, entV ! i2) of
          (Just e1, Just e2) -> interact_ True (e1,e2) >>= \(e1',e2') -> return (entV // [(i1,e1'),(i2,e2')])
          _ -> return entV
    v <- foldM go entityVector collisions -- handle interactions
    mapM (maybe (return Nothing) step) v  -- step each entity

  step :: Entity -> CM (Maybe Entity)
  step = \case
    EPlayer a -> update EPlayer a $ stepPlayer input
    EBullet a -> update EBullet a $ stepBullet time dtime
    PSpawn  a -> update PSpawn a $ stepSpawn time dtime
    e -> return $ Just e

  interact_ :: Bool -> (Entity,Entity) -> CM (Maybe Entity,Maybe Entity) -- TODO: generalize pairs to interaction event types
                                                                        --        e.g. player-item, player-teleport, killbox-players
  interact_ swap = \case
    -- TODO: filter interactions, maybe name them: pairs, teleport
    -- Interact only if a player A <> teleport && player B <> killbox
    (EPlayer p,EKillbox a)  -> (,) <$> update EPlayer p (playerDie time) <*> update EKillbox a (return ())
    -- HINT: teleport targets should be passed here
    --        where to collect those?
    (EPlayer p,ETeleport a) -> (,) <$> update EPlayer p (pPosition .= head [t^.ttPosition | ETarget t <- ents, t^.ttTargetName == a^.tTarget]) -- TODO: lookup target, get the position + implement telefrag (killbox)
                                   <*> update ETeleport a (return ())


    (EPlayer p,EHealth a) -> do -- collects newly create entities also handles random seed
                              let c = p^.pHealth >= 200
                              (,) <$> update EPlayer p (unless c $ pHealth += a^.hQuantity) -- can: die or create new entities
                                  <*> update EHealth a (unless c $ respawn time EHealth)

    (EPlayer p,EBullet a) -> (,) <$> update EPlayer p (pHealth -= a^.bDamage)
                                 <*> update EBullet a die

    (EPlayer p,EArmor a)  -> (,) <$> update EPlayer p (do {pArmor += a^.rQuantity; pArmorType .= Just (a^.rType)})
                                 <*> update EArmor a (if a^.rDropped then die else respawn time EArmor)

    (EPlayer p,EAmmo a)   -> (,) <$> update EPlayer p (pickUpAmmo (a ^. aQuantity) (a ^. aType))
                                 <*> update EAmmo a (if a^.aDropped then die else respawn time EAmmo)

    (EPlayer p,EWeapon a) -> (,) <$> update EPlayer p (do pickUpAmmo (if (a ^. wDropped) then 0 else 10) (a ^. wType)
                                                          pickUpWeapon (a ^. wType))
                                 <*> update EWeapon a (if a^.wDropped then die else respawn time EWeapon)

    (EPlayer p,ELava a)   -> (,) <$> update EPlayer p (do {tick <- oncePerSec; when tick (pHealth -= a^.lDamage)})
                                 <*> update ELava a (return ())

    (EPlayer p,EHoldable h) -> (,) <$> update EPlayer p (do let Just period = Map.lookup (h ^. hoType) Items.holdableTime
                                                            pHoldables %= Map.insert (h ^. hoType) (False, period))
                                   <*> update EHoldable h (respawn time EHoldable)

    (EPlayer p,EPowerup pu) -> (,) <$> update EPlayer p (pPowerups %= Set.insert (pu ^. puType))
                                   <*> update EPowerup pu (respawn time EPowerup)

    (EBullet a,b)         -> (,Just b) <$> update EBullet a die -- bug in this case: (EBullet,EPlayer)

    (a,b) | swap -> interact_ False (b,a)
          | otherwise -> return (Just a,Just b)

  pickUpAmmo q w = pAmmos %= Map.insertWith (+) w q
  pickUpWeapon w = pWeapons %= Set.insert w

  oncePerSec = do
    t <- use pDamageTimer
    if t > time then return False else do
      pDamageTimer .= time + 1
      return True
-}

applyWorldRules :: World -> World
applyWorldRules = Player.spawnPlayer

instance Mover Bullet where
  getPosition = use bPosition
  getVelocity = use bDirection
  reactToCollision _ _ = die
  reactToGroundHit _ _ = die
  reactToFalling = return ()
  setPosition newPos = bPosition .= newPos
  setVelocity newVel = bDirection .= newVel 
  getBounds = return $ (,) (Vec3 (-3) (-3) (-3)) (Vec3 3 3 3)
  
stepRocket :: EntityM Bullet Bullet
stepRocket = do
 Input{..} <- userInput <$> ask
 dir       <- use bDirection
 tryMovingWithRay (bPosition += dtime *& dir) reactToCollision
 bLifeTime -= dtime
 lifeTime <- use bLifeTime
 when (lifeTime < 0) die
 get
 
 

updateVisuals :: PureMT -> Time -> DTime -> [Visual] -> (PureMT,[Visual])
updateVisuals randGen time dtime visuals = (randGen',catMaybes visuals' ++ newVisuals) where
  ((visuals',newVisuals),randGen') = collect randGen $ mapM stepVisual visuals

  stepVisual = \case
    VParticle a -> update VParticle a $ stepParticle time dtime
    e -> return $ Just e

stepParticle :: Time -> DTime -> VM Particle ()
stepParticle t dt = do
  dir <- use vpDirection
  vpPosition += dt *& dir
  -- die on spent lifetime
  vpLifeTime -= dt
  lifeTime <- use vpLifeTime
  when (lifeTime < 0) die

-- world step function
stepFun :: BSPLevel -> ResourceCache -> RenderSystem -> DTime -> World -> World
stepFun map resources engine dt = execState $ do
  -- update time
  wInput %= (\i -> i {dtime = dt, time = time i + dt}) --update time and deltatime
  input <- use wInput
  ents <- use wEntities
  vis <- use wVisuals
  rand <- use wRandomGen
  let (r1,e,v1) = updateScene map resources engine rand input ents
      Input{..} = input
      (r2,v2) = updateVisuals r1 time dtime vis --update visual effects
  wEntities .= e
  wRandomGen .= r2
  wVisuals .= v1 ++ v2
  modify applyWorldRules

logPlayerChange :: World -> World -> Maybe String
logPlayerChange old new = do
  op <- old ^. player
  np <- new ^. player
  guard (nullSomeValues (fromEPlayer op) /= nullSomeValues (fromEPlayer np))
  pure $ show np
  where
    isPlayer (EPlayer _) = True
    isPlayer _           = False
    player = wEntities . to (find isPlayer)
    fromEPlayer (EPlayer p) = p
    nullSomeValues =
      set pPosition (Vec3 0 0 0)  .
      set pDirection (Vec3 0 0 0) .
      set pVelocity (Vec3 0 0 0)  .
      set pShootTime 0

-- FIXME: Order
weaponKeys :: Map Int Items.Weapon
weaponKeys = Map.fromList
  [ (0, Items.WP_GAUNTLET)
  , (1, Items.WP_MACHINEGUN)
  , (2, Items.WP_SHOTGUN)
  , (3, Items.WP_GRENADE_LAUNCHER)
  , (4, Items.WP_ROCKET_LAUNCHER)
  , (5, Items.WP_LIGHTNING)
  , (6, Items.WP_RAILGUN)
  , (7, Items.WP_PLASMAGUN)
  , (8, Items.WP_BFG)
  , (9, Items.WP_GRAPPLING_HOOK)
  ]

holdableKeys :: Map Int Items.Holdable
holdableKeys = Map.fromList
  [ (0, Items.HI_TELEPORTER)
  , (1, Items.HI_MEDKIT)
  , (2, Items.HI_KAMIKAZE)
  , (3, Items.HI_PORTAL)
  , (4, Items.HI_INVULNERABILITY)
  ]
 
 
class Spawnable object where
 setSpawnTime :: Time -> object -> object
 
instance Spawnable Entity where
 setSpawnTime t = \case
   EWeapon a   -> EWeapon   (a & wTime  .~ t)
   EAmmo a     -> EAmmo     (a & aTime  .~ t)
   EArmor a    -> EArmor    (a & rTime  .~ t)
   EHealth a   -> EHealth   (a & hTime  .~ t)
   EHoldable a -> EHoldable (a & hoTime .~ t)
   EPowerup p  -> EPowerup  (p & puTime .~ t)
   e           -> e
   
  
updateEntity :: EntityEnvironment -> (e -> Entity) -> e -> EntityM e e -> CollectM ()
updateEntity entityEnv transformation initialState action = do
  seed <- getRandom
  let (modifiedEntity, newObjects) = runEntityM entityEnv initialState action (pureMT $ fromIntegral (seed :: Int))
  maybe (return ()) (addEntity . transformation) modifiedEntity
  tell newObjects
  
  
applyAction :: EntityEnvironment -> Action -> Entity -> Maybe Entity
applyAction env (Damage dmg) (EPlayer p)  = Just $ EPlayer $ p { _pHealth = _pHealth p - dmg } 
applyAction _ _ _ = undefined
 
updateScene :: BSPLevel -> ResourceCache -> RenderSystem -> PureMT -> Input -> [Entity] -> (PureMT,[Entity],[Visual])
updateScene map resourceCache engine randGen input ents = (newRandGen, actionProcessedEntites, newVisuals) 
  where
    entityEnv = EntityEnvironment 
     { 
       resources = resourceCache
     , level     = map 
     , userInput = input
     , gravity   = Vec3 0 0 (-15.6)
     , entities  = entityVector
     }
    
    entityVector :: Vector Entity
    entityVector = V.fromList ents

    collisions :: [(Int,Int)]
    collisions = getCollisions engine ents

    interactions :: [(Int,Int)] -> [Interaction]
    interactions _ = [] 
    
    interact_ :: Bool -> (Entity, Entity) -> (Maybe Entity, Maybe Entity)
    interact_ _ (e1, e2) = (Just e1, Just e2)
    
    entities_after_interaction = V.mapMaybe id $ V.modify (\mvector -> mapM_ (go mvector) collisions) $ V.map Just entityVector
    writeUpdatedEnts mvector i1 i2 (e1, e2) = MV.write mvector i1 e1 >> MV.write mvector i2 e2
    go mvector (i1,i2) = ((,) <$> MV.read mvector i1 <*> MV.read mvector i2) >>= \case 
     (Just e1, Just e2) -> writeUpdatedEnts mvector i1 i2 (interact_ True (e1,e2)) 
     _ -> return ()
    
    applyActions ents actions = ents // fmap (apply ents) actions 
    
    apply ents (index, action) = let entity = ents ! index in (index, maybe Nothing (applyAction actionEnv action) entity)
    
    ((entitiesInNextFrame, newVisuals, actions), newRandGen) = runCollectM randGen $ mapM_ step entities_after_interaction
    actionProcessedEntites = V.toList $ V.mapMaybe id $ applyActions (V.map Just $ entitiesInNextFrameAsVector) actions
    
    entitiesInNextFrameAsVector = V.fromList entitiesInNextFrame
    actionEnv = entityEnv { entities =  entitiesInNextFrameAsVector }
      
    update = updateEntity $ entityEnv { entities = entities_after_interaction }

    step :: Entity -> CollectM ()
    step (EPlayer player) = update EPlayer player Player.stepPlayer
    step (PSpawn spawn) = update PSpawn spawn stepSpawn
    step (EBullet bullet) = update EBullet bullet stepRocket
    step x = addEntity x 
    
    stepSpawn :: EntityM Spawn Spawn
    stepSpawn = do
     let 
      spawnEntity t = do
       ent <- setSpawnTime t <$> use sEntity
       addEntity ent
     spawnTime <- use sSpawnTime
     t <- time . userInput <$> ask
     if t < spawnTime then get else spawnEntity t >> die
   
     
    



