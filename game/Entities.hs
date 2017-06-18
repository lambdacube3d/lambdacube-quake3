{-# LANGUAGE TemplateHaskell #-}
module Entities where

import Data.Map.Strict
import Data.Set
import Data.Vect
import Data.Vect.Float.Util.Quaternion
import Lens.Micro.Platform
import qualified Items

-- entities for game logic

data Player
  = Player
  { _pPosition    :: Vec3
  , _pDirection   :: Vec3
  , _pFVelocity   :: Float
  , _pSVelocity   :: Float
  , _pHealth      :: Int
  , _pArmor       :: Int
  , _pShootTime   :: Float
  , _pDamageTimer :: Float
  , _pName        :: String
  , _pId          :: Int
  , _pAmmos       :: Map Items.Weapon Int
  , _pWeapons     :: Set Items.Weapon
  , _pSelectedWeapon :: Items.Weapon
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
  , _wType        :: Items.Weapon
  } deriving Show

data Ammo
  = Ammo
  { _aPosition    :: Vec3
  , _aQuantity    :: Int
  , _aDropped     :: Bool
  , _aType        :: Items.Weapon
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

data SpawnPoint
  = SpawnPoint
  { _spPosition :: Vec3
  , _spAngles   :: Vec3
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
  = EPlayer     Player
  | EBullet     Bullet
  | EWeapon     Weapon
  | EAmmo       Ammo
  | EArmor      Armor
  | EHealth     Health
  | ELava       Lava
  | ETeleport   Teleport
  | ETarget     Target
  | EKillbox    Killbox
  | PSpawn      Spawn
  | ESpawnPoint SpawnPoint
  deriving Show

concat <$> mapM makeLenses [''Player, ''Bullet, ''Weapon, ''Ammo, ''Armor, ''Spawn, ''Health, ''Lava, ''Teleport, ''Target, ''Killbox]
