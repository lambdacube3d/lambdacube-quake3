{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, NoMonomorphismRestriction, FlexibleContexts #-}
module GameLogic where

import Control.Monad
import Data.Maybe
import System.Random.Mersenne.Pure64
import Lens.Micro.Platform
import Data.Vect
import Data.Vect.Float.Instances

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Control.Monad.Random
import System.Random.Mersenne.Pure64

import Data.Vector ((!),(//))
import qualified Data.Vector as V

import Entities
import Visuals
import World
--import Collision

type Time = Float
type DTime = Float

-- entity update monad (mutable state + collect new entities/visuals)
type EM s a = ReaderT s (StateT s (MaybeT (WriterT ([Entity],[Visual]) (Rand PureMT)))) a

-- visual item update monad + collect new visual items
type VM s a = ReaderT s (StateT s (MaybeT (WriterT ([Visual]) (Rand PureMT)))) a

-- monad for collect new entites or visuals
type CM a = WriterT ([Entity],[Visual]) (Rand PureMT) a

die = fail "die"

respawn t f = do
  s <- get
  t' <- getRandomR (t + 5, t + 10)
  addEntities [PSpawn $ Spawn t' (f s)]
  die

--update :: Monoid w => (s -> e) -> s -> ReaderT s (StateT s (MaybeT (WriterT w (Rand PureMT)))) a -> CM (Maybe e)
update f a m = fmap f <$> runMaybeT (execStateT (runReaderT m a) a)

collect :: Monoid w => PureMT -> WriterT w (Rand PureMT) a -> ((a,w),PureMT)
collect randGen m = runIdentity $ runRandT (runWriterT m) randGen

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
updateEntities :: PureMT -> Input -> [Entity] -> (PureMT,[Entity],[Visual])
updateEntities randGen input@Input{..} ents = (randGen',catMaybes (V.toList nextEnts) ++ newEnts,newVisuals) where

  entityVector :: V.Vector (Maybe Entity)
  entityVector = V.fromList $ map Just ents

  collisions :: [(Int,Int)] -- post process collisions into interaction events
  collisions = [] -- TODO: collide ents

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

    (EPlayer p,EArmor a)  -> (,) <$> update EPlayer p (pArmor += a^.rQuantity)
                                 <*> update EArmor a (if a^.rDropped then die else respawn time EArmor)

    (EPlayer p,EAmmo a)   -> (,) <$> update EPlayer p (pAmmo += a^.aQuantity)
                                 <*> update EAmmo a (if a^.aDropped then die else respawn time EAmmo)

    (EPlayer p,EWeapon a) -> (,) <$> update EPlayer p (pAmmo += 10)
                                 <*> update EWeapon a (if a^.wDropped then die else respawn time EWeapon)

    (EPlayer p,ELava a)   -> (,) <$> update EPlayer p (do {tick <- oncePerSec; when tick (pHealth -= a^.lDamage)})
                                 <*> update ELava a (return ())

    (EBullet a,b)         -> (,Just b) <$> update EBullet a die -- bug in this case: (EBullet,EPlayer)

    (a,b) | swap -> interact_ False (b,a)
          | otherwise -> return (Just a,Just b)

  oncePerSec = do
    t <- use pDamageTimer
    if t > time then return False else do
      pDamageTimer .= time + 1
      return True

initialPlayer = Player
  { _pPosition    = Vec2 0 0
  , _pFVelocity   = 0
  , _pSVelocity   = 0
  , _pAngle       = 0
  , _pHealth      = 100
  , _pAmmo        = 100
  , _pArmor       = 0
  , _pShootTime   = 0
  , _pDamageTimer = 0
  , _pName        = "Bones"
  , _pId          = 0
  }

addEntities ents = tell (ents,[])
addVisuals vis = tell ([],vis)

stepSpawn :: Time -> DTime -> EM Spawn ()
stepSpawn t dt = do
  spawnTime <- view sSpawnTime
  unless (t < spawnTime) $ do
    ent <- view sEntity
    addEntities [ent]
    die

stepPlayer :: Input -> EM Player ()
stepPlayer input@Input{..} = do
  -- acceleration according input
  pAngle += rightmove * dtime
  angle <- use pAngle
  let direction = unitVectorAtAngle $ degToRad angle
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
  let strafeDirection = unitVectorAtAngle $ degToRad (angle - 90)
  pPosition += (dtime * sideVelocity) *& strafeDirection
  -- shoot
  shootTime <- view pShootTime
  when (shoot && shootTime < time) $ do
    pos <- use pPosition
    addEntities [EBullet $ Bullet (pos + 30 *& direction) (500 *& direction) 1 2]
    pShootTime .= time + 0.1

  pHealth %= min 200
  -- death
  health <- use pHealth
  unless (health > 0) $ playerDie time

playerDie time = do
    [x1,y1,x2,y2] <- replicateM 4 $ getRandomR (-50,50)
    pos <- use pPosition
    ammo <- use pAmmo
    armor <- use pArmor
    addEntities
      [ PSpawn $ Spawn (time + 2) $ EPlayer initialPlayer
      , EAmmo  $ Ammo (pos + Vec2 x1 y1) (ammo) True
      , EArmor $ Armor (pos + Vec2 x2 y2) (armor) True
      ]
    addVisuals [VParticle $ Particle pos (400 *& unitVectorAtAngle (pi / 50 * i)) 1 | i <- [0..100]]
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
stepFun :: Float -> World -> World
stepFun dt = execState $ do
  -- update time
  wInput %= (\i -> i {dtime = dt, time = time i + dt})
  input <- use wInput
  ents <- use wEntities
  vis <- use wVisuals
  rand <- use wRandomGen
  let (r1,e,v1) = updateEntities rand input ents
      Input{..} = input
      (r2,v2) = updateVisuals r1 time dtime vis
  wEntities .= e
  wRandomGen .= r2
  wVisuals .= v1 ++ v2
