{-# LANGUAGE TemplateHaskell #-}
module Entities where

import Data.Vect
import Lens.Micro.Platform

-- entities for game logic

data Player
  = Player
  { _pPosition    :: Vec3
  , _pFVelocity   :: Float
  , _pSVelocity   :: Float
  , _pAngle       :: Float
  , _pHealth      :: Int
  , _pAmmo        :: Int
  , _pArmor       :: Int
  , _pShootTime   :: Float
  , _pDamageTimer :: Float
  , _pName        :: String
  , _pId          :: Int
  } deriving Show

data Bullet
  = Bullet
  { _bPosition    :: Vec3
  , _bDirection   :: Vec3
  , _bDamage      :: Int
  , _bLifeTime    :: Float
  } deriving Show

data Weapon
  = Weapon
  { _wPosition    :: Vec3
  , _wDropped     :: Bool
  } deriving Show

data Ammo
  = Ammo
  { _aPosition    :: Vec3
  , _aQuantity    :: Int
  , _aDropped     :: Bool
  } deriving Show

data Armor
  = Armor
  { _rPosition    :: Vec3
  , _rQuantity    :: Int
  , _rDropped     :: Bool
  } deriving Show

data Health
  = Health
  { _hPosition   :: Vec3
  , _hQuantity   :: Int
  } deriving Show

data Spawn
  = Spawn
  { _sSpawnTime :: Float
  , _sEntity    :: Entity
  } deriving Show

data Lava
  = Lava
  { _lPosition :: Vec3
  , _lDamage   :: Int
  } deriving Show

data Teleport
  = Teleport
  { _tPosition  :: Vec3
  , _tTarget    :: String
  } deriving Show

data Target
  = Target
  { _ttPosition   :: Vec3
  , _ttTargetName :: String
  } deriving Show

data Killbox
  = Killbox
  { _kPosition    :: Vec3
  , _kTargetName  :: String
  } deriving Show

data Entity
  = EPlayer   Player
  | EBullet   Bullet
  | EWeapon   Weapon
  | EAmmo     Ammo
  | EArmor    Armor
  | EHealth   Health
  | ELava     Lava
  | ETeleport Teleport
  | ETarget   Target
  | EKillbox  Killbox
  | PSpawn    Spawn
  deriving Show

concat <$> mapM makeLenses [''Player, ''Bullet, ''Weapon, ''Ammo, ''Armor, ''Spawn, ''Health, ''Lava, ''Teleport, ''Target, ''Killbox]
