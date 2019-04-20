{-# LANGUAGE TemplateHaskell, DeriveGeneric, StandaloneDeriving #-}
module Entities where

import GHC.Generics (Generic)
import Data.Binary

import Data.Map.Strict
import Data.Set
import Data.Vect
import Data.Vect.Float.Util.Quaternion
import Data.Vect.Float.Instances
import Lens.Micro.Platform
import qualified Items

-- entities for game logic

data Player
  = Player
  { _pPosition    :: Vec3
  , _pDirection   :: Vec3
  , _pVelocity    :: Vec3
  , _pRotationUV  :: Vec3
  , _pHealth      :: Int
  , _pArmor       :: Int
  , _pArmorType   :: Maybe Items.Armor
  , _pShootTime   :: Float
  , _pDamageTimer :: Float
  , _pName        :: String
  , _pId          :: Int
  , _pAmmos       :: Map Items.Weapon Int
  , _pWeapons     :: Set Items.Weapon
  , _pSelectedWeapon :: Items.Weapon
  , _pHoldables   :: Map Items.Holdable (Bool, Float)
  , _pPowerups    :: Set Items.Powerup
  , _pMD3Model    :: String
  , _pModelFrame  :: Maybe Int
  } deriving (Eq, Show, Generic)

data Bullet
  = Bullet
  { _bPosition    :: Vec3
  , _bDirection   :: Vec3
  , _bDamage      :: Int
  , _bLifeTime    :: Float
  , _bType        :: Items.Weapon
  } deriving (Eq, Show, Generic)

data Weapon
  = Weapon
  { _wPosition    :: Vec3
  , _wDropped     :: Bool
  , _wType        :: Items.Weapon
  , _wTime        :: Float
  } deriving (Eq, Show, Generic)

data Ammo
  = Ammo
  { _aPosition    :: Vec3
  , _aQuantity    :: Int
  , _aDropped     :: Bool
  , _aType        :: Items.Weapon
  , _aTime        :: Float
  } deriving (Eq, Show, Generic)

data Armor
  = Armor
  { _rPosition    :: Vec3
  , _rQuantity    :: Int
  , _rDropped     :: Bool
  , _rType        :: Items.Armor
  , _rTime        :: Float
  } deriving (Eq, Show, Generic)

data Health
  = Health
  { _hPosition   :: Vec3
  , _hQuantity   :: Int
  , _hType       :: Items.Health
  , _hTime       :: Float
  } deriving (Eq, Show, Generic)

data Spawn
  = Spawn
  { _sSpawnTime :: Float
  , _sEntity    :: Entity
  } deriving (Eq, Show, Generic)

data SpawnPoint
  = SpawnPoint
  { _spPosition :: Vec3
  , _spAngles   :: Vec3
  } deriving (Eq, Show, Generic)

data Lava
  = Lava
  { _lPosition :: Vec3
  , _lDamage   :: Int
  } deriving (Eq, Show, Generic)

data Teleport
  = Teleport
  { _tPosition  :: Vec3
  , _tTarget    :: String
  } deriving (Eq, Show, Generic)

data Target
  = Target
  { _ttPosition   :: Vec3
  , _ttTargetName :: String
  } deriving (Eq, Show, Generic)

data Killbox
  = Killbox
  { _kPosition    :: Vec3
  , _kTargetName  :: String
  } deriving (Eq, Show, Generic)

data Holdable
  = Holdable
  { _hoPosition :: Vec3
  , _hoType     :: Items.Holdable
  , _hoTime     :: Float
  } deriving (Eq, Show, Generic)

data Powerup
  = Powerup
  { _puPosition :: Vec3
  , _puType     :: Items.Powerup
  , _puTime     :: Float
  } deriving (Eq, Show, Generic)

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
  | EHoldable   Holdable
  | EPowerup    Powerup
  deriving (Eq, Show, Generic)
  
isPlayer :: Entity -> Bool
isPlayer (EPlayer _) = True
isPlayer _ = False
  
{-
Role of EM:
Entities get updated in this monad.
The final output is a pair of entities and visuals to render next frame.
It features mutable state for manipulating an entity before adding it to the output.
For example: updatePlayer :: EM Player ()  
-}
  
class Entity' a where
-- think :: CM 
  
{-
typedef enum {
	ET_GENERAL,
	ET_PLAYER,
	ET_ITEM,
	ET_MISSILE,
	ET_MOVER,
	ET_BEAM,
	ET_PORTAL,
	ET_SPEAKER,
	ET_PUSH_TRIGGER,
	ET_TELEPORT_TRIGGER,
	ET_INVISIBLE,
	ET_GRAPPLE,				// grapple hooked on wall
	ET_TEAM,

	ET_EVENTS				// any of the EV_* events can be added freestanding
							// by setting eType to ET_EVENTS + eventNum
							// this avoids having to set eFlags and eventNum
} entityType_t; 

typedef enum {
	IT_BAD,
	IT_WEAPON,				// EFX: rotate + upscale + minlight
	IT_AMMO,				// EFX: rotate
	IT_ARMOR,				// EFX: rotate + minlight
	IT_HEALTH,				// EFX: static external sphere + rotating internal
	IT_POWERUP,				// instant on, timer based
							// EFX: rotate + external ring that rotates
	IT_HOLDABLE,			// single use, holdable item
							// EFX: rotate + bob
	IT_PERSISTANT_POWERUP,
	IT_TEAM
} itemType_t;


-}

concat <$> mapM makeLenses [''Player, ''Bullet, ''Weapon, ''Ammo, ''Armor, ''Spawn, ''Health, ''Lava, ''Teleport, ''Target, ''Killbox, ''Holdable, ''Powerup]

deriving instance Generic Vec3
instance Binary Vec3

instance Binary Player
instance Binary Bullet
instance Binary Weapon
instance Binary Ammo
instance Binary Armor
instance Binary Spawn
instance Binary SpawnPoint
instance Binary Health
instance Binary Lava
instance Binary Teleport
instance Binary Target
instance Binary Killbox
instance Binary Holdable
instance Binary Powerup
instance Binary Entity
