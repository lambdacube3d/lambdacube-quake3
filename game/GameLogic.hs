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
import Data.Vect.Float.Util.Quaternion

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Control.Monad.Random
import System.Random.Mersenne.Pure64

import Data.Vector (Vector,(!),(//))
import qualified Data.Vector as V

import GameEngine.RenderSystem

import qualified Items
import Entities
import Visuals
import World
import Collision
import qualified Player


type Time = Float
type DTime = Float

-- entity update monad (mutable state + collect new entities/visuals)
type EM s a = ReaderT s (StateT s (MaybeT (WriterT ([Entity],[Visual]) (Rand PureMT)))) a

-- visual item update monad + collect new visual items
type VM s a = ReaderT s (StateT s (MaybeT (WriterT ([Visual]) (Rand PureMT)))) a

-- monad for collect new entites or visuals
type CM a = WriterT ([Entity],[Visual]) (Rand PureMT) a

--update :: Monoid w => (s -> e) -> s -> ReaderT s (StateT s (MaybeT (WriterT w (Rand PureMT)))) a -> CM (Maybe e)
update f a m = fmap f <$> runMaybeT (execStateT (runReaderT m a) a)

collect :: Monoid w => PureMT -> WriterT w (Rand PureMT) a -> ((a,w),PureMT)
collect randGen m = runIdentity $ runRandT (runWriterT m) randGen

die :: Monad m => ReaderT s (StateT s (MaybeT m)) a
die = mzero

respawn t f = do
  s <- get
  t' <- getRandomR (t + 5, t + 10)
  addEntities [PSpawn $ Spawn t' (f s)]
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
-}
updateEntities :: RenderSystem -> PureMT -> Input -> [Entity] -> (PureMT,[Entity],[Visual])
updateEntities engine randGen input@Input{..} ents = (randGen',catMaybes (V.toList nextEnts) ++ newEnts,newVisuals) where

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

  pickUpAmmo q w = pAmmos %= Map.adjust ((+) q) w
  pickUpWeapon w = pWeapons %= Set.insert w

  oncePerSec = do
    t <- use pDamageTimer
    if t > time then return False else do
      pDamageTimer .= time + 1
      return True

applyWorldRules :: World -> World
applyWorldRules = spawnPlayer

-- | Spawn a player if there is none.
spawnPlayer :: World -> World
spawnPlayer w = w { _wEntities = entities }
  where
    wTime = w ^. wInput . to time
    entities = (case find hasPlayer (_wEntities w) of
                  Nothing -> ((PSpawn . Spawn (wTime + 2) $ EPlayer player):)
                  Just _  -> id) $ _wEntities w

    hasPlayer (EPlayer _)                    = True
    hasPlayer (PSpawn (Spawn _ (EPlayer _))) = True
    hasPlayer _                              = False

    isSpawnPoint (ESpawnPoint _) = True
    isSpawnPoint _               = False

    (ESpawnPoint spawnPoint) = fromJust $ find isSpawnPoint (_wEntities w)
    player = Player
      { _pPosition    = _spPosition spawnPoint
      , _pDirection   = _spAngles spawnPoint
      , _pFVelocity   = 0
      , _pSVelocity   = 0
      , _pHealth      = 100
      , _pArmor       = 0
      , _pArmorType   = Nothing
      , _pShootTime   = 0
      , _pDamageTimer = 0
      , _pName        = "Bones"
      , _pId          = 0
      , _pWeapons     = Set.fromList [Items.WP_GAUNTLET, Items.WP_MACHINEGUN]
      , _pSelectedWeapon = Items.WP_MACHINEGUN
      , _pAmmos       = Map.fromList
           [ (Items.WP_GAUNTLET,         1)
           , (Items.WP_MACHINEGUN,     100)
           , (Items.WP_SHOTGUN,          0)
           , (Items.WP_GRENADE_LAUNCHER, 0)
           , (Items.WP_ROCKET_LAUNCHER,  0)
           , (Items.WP_LIGHTNING,        0)
           , (Items.WP_RAILGUN,          0)
           , (Items.WP_PLASMAGUN,        0)
           , (Items.WP_BFG,              0)
           ]
      , _pHoldables  = Map.empty
      , _pPowerups   = Set.empty
      }

addEntities ents = tell (ents,[])
addVisuals vis = tell ([],vis)

stepSpawn :: Time -> DTime -> EM Spawn ()
stepSpawn t dt = do
  spawnTime <- view sSpawnTime
  unless (t < spawnTime) $ do
    ent <- setSpawnTime <$> view sEntity
    addEntities [ent]
    die
  where
    setSpawnTime = \case
      EWeapon a   -> EWeapon   (a & wTime  .~ t)
      EAmmo a     -> EAmmo     (a & aTime  .~ t)
      EArmor a    -> EArmor    (a & rTime  .~ t)
      EHealth a   -> EHealth   (a & hTime  .~ t)
      EHoldable a -> EHoldable (a & hoTime .~ t)
      EPowerup p  -> EPowerup  (p & puTime .~ t)
      e           -> e

stepPlayer :: Input -> EM Player ()
stepPlayer input@Input{..} = do
  -- acceleration according input
  pDirection .= normalize (Vec3 0 0 (sin $ mouseY / 100) + rotU (Vec3 0 0 1) (-mouseX / 100) *. Vec3 1 0 0)
  direction <- use pDirection
  let up = Vec3 0 0 1
      forward = Vec3 1 0 0
      strafeDirection = normalize $ direction `crossprod` up

  pFVelocity += forwardmove * dtime
  pSVelocity += sidemove * dtime
  -- friction
  len <- use pFVelocity
  sideLen <- use pSVelocity
  let friction = 150
  pFVelocity %= (*) (max 0 $ (len - dtime * friction * signum len) / len)
  pSVelocity %= (*) (max 0 $ (sideLen - dtime * friction * signum sideLen) / sideLen)

  -- move
  pFVelocity %= max (-200) . min 200
  pSVelocity %= max (-200) . min 200
  forwardVelocity <- use pFVelocity
  sideVelocity <- use pSVelocity

  pPosition += (dtime * forwardVelocity) *& direction
  pPosition += (dtime * sideVelocity) *& strafeDirection
  
  Player.changeWeapon input
  Player.shoots input
  Player.togglesHoldable input
  Player.tickHoldables input

  pHealth %= min 200
  -- death
  health <- use pHealth
  unless (health > 0) $ playerDie time

playerDie time = do
    pos <- use pPosition
    ammos     <- Map.toList <$> use pAmmos
    armor     <- use pArmor
    armorType <- use pArmorType
    weapons <- Set.toList <$> use pWeapons
    let randomPos = (pos +) <$> getRandomR (Vec3 (-50) (-50) (-50), Vec3 50 50 50) 
    droppedAmmos <- forM ammos $ \(weapon, amount) -> do
      rpos <- randomPos
      return $ EAmmo $ Ammo rpos amount True weapon time
    droppedWeapos <- forM weapons $ \weapon -> do
      rpos <- randomPos
      return $ EWeapon $ Weapon rpos True weapon time
    droppedArmors <- case armorType of
      Just at | armor > 0 -> do
        rpos <- randomPos
        return [(EArmor $ Armor rpos armor True at time)]
      _ -> return []
    addEntities $ concat [droppedArmors, droppedWeapos, droppedAmmos]
    addVisuals [VParticle $ Particle pos (400 *& (extendZero . unitVectorAtAngle $ pi / 50 * i)) 1 | i <- [0..100]]
    die

stepBullet :: Time -> DTime -> EM Bullet ()
stepBullet t dt = do
  dir <- use bDirection
  bPosition += dt *& dir
  -- die on spent lifetime
  bLifeTime -= dt
  lifeTime <- use bLifeTime
  when (lifeTime < 0) die

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

-- utils
unitVectorAtAngle = sinCos
degToRad a = a/180*pi

-- world step function
stepFun :: RenderSystem -> Float -> World -> World
stepFun engine dt = execState $ do
  -- update time
  wInput %= (\i -> i {dtime = dt, time = time i + dt})
  input <- use wInput
  ents <- use wEntities
  vis <- use wVisuals
  rand <- use wRandomGen
  let (r1,e,v1) = updateEntities engine rand input ents
      Input{..} = input
      (r2,v2) = updateVisuals r1 time dtime vis
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
      set pFVelocity 0            .
      set pSVelocity 0            .
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
