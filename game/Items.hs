{-# LANGUAGE DeriveGeneric #-}
module Items where

import GHC.Generics (Generic)
import Data.Binary
import qualified Data.IntMap as IM

import Data.Map.Strict as Map

-- | Items that became active when the player pick them up.
data Powerup
  = PW_QUAD
  | PW_BATTLESUIT
  | PW_HASTE
  | PW_INVIS
  | PW_REGEN
  | PW_FLIGHT

  | PW_SCOUT
  | PW_GUARD
  | PW_DOUBLER
  | PW_AMMOREGEN
  | PW_INVULNERABILITY
  deriving (Eq, Ord, Show, Generic)

data Team
  = PW_REDFLAG
  | PW_BLUEFLAG
  | PW_NEUTRALFLAG
  deriving (Eq, Ord, Show, Generic)

-- | Items that the player can hold and activate, deactivate.
data Holdable
  = HI_TELEPORTER
  | HI_MEDKIT
  | HI_KAMIKAZE
  | HI_PORTAL
  | HI_INVULNERABILITY
  deriving (Eq, Ord, Show, Generic)

data Armor
  = AR_SHARD
  | AR_COMBAT
  | AR_BODY
  deriving (Eq, Ord, Show, Generic)

data Health
  = HE_SMALL
  | HE_MEDIUM
  | HE_LARGE
  | HE_MEGA
  deriving (Eq, Ord, Show, Generic)

data Weapon
  = WP_GAUNTLET
  | WP_MACHINEGUN
  | WP_SHOTGUN
  | WP_GRENADE_LAUNCHER
  | WP_ROCKET_LAUNCHER
  | WP_LIGHTNING
  | WP_RAILGUN
  | WP_PLASMAGUN
  | WP_BFG
  | WP_GRAPPLING_HOOK
  deriving (Eq, Ord, Show, Generic, Enum)

instance Binary Powerup
instance Binary Team
instance Binary Holdable
instance Binary Armor
instance Binary Health
instance Binary Weapon

data ItemType
  = IT_WEAPON             Weapon    -- EFX: rotate + upscale + minlight
  | IT_AMMO               Weapon    -- EFX: rotate
  | IT_ARMOR              Armor     -- EFX: rotate + minlight
  | IT_HEALTH             Health    -- EFX: static external sphere + rotating internal
  | IT_POWERUP            Powerup   -- instant on, timer based; EFX: rotate + external ring that rotates
  | IT_HOLDABLE           Holdable  -- single use, holdable item; EFX: rotate + bob
  | IT_PERSISTANT_POWERUP
  | IT_TEAM               Team
  deriving (Eq, Ord, Show)

data Item
  = Item
  { itClassName   :: String
  , itPickupSound :: Maybe String
  , itWorldModel  :: [String]
  , itIcon        :: String
  , itPickupName  :: String
  , itQuantity    :: Int
  , itType        :: ItemType
  , itSounds      :: [String]
  } deriving Show

holdableTime :: Map Holdable Float
holdableTime = Map.fromList
  [ (HI_TELEPORTER, 10)
  , (HI_MEDKIT, 10)
  , (HI_KAMIKAZE, 10)
  , (HI_PORTAL, 10)
  , (HI_INVULNERABILITY, 10)
  ]

items :: [Item]
items =
  [ Item
    { itClassName   = "item_armor_shard"
    , itPickupSound = Just "sound/misc/ar1_pkup.wav"
    , itWorldModel  = ["models/powerups/armor/shard.md3"]
    , itIcon        = "icons/iconr_shard"
    , itPickupName  = "Armor Shard"
    , itQuantity    = 5
    , itType        = IT_ARMOR AR_SHARD
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_armor_combat"
    , itPickupSound = Just "sound/misc/ar2_pkup.wav"
    , itWorldModel  = ["models/powerups/armor/armor_yel.md3"]
    , itIcon        = "icons/iconr_yellow"
    , itPickupName  = "Armor"
    , itQuantity    = 50
    , itType        = IT_ARMOR AR_COMBAT
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_armor_body"
    , itPickupSound = Just "sound/misc/ar2_pkup.wav"
    , itWorldModel  = ["models/powerups/armor/armor_red.md3"]
    , itIcon        = "icons/iconr_red"
    , itPickupName  = "Heavy Armor"
    , itQuantity    = 100
    , itType        = IT_ARMOR AR_BODY
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_health_small"
    , itPickupSound = Just "sound/items/s_health.wav"
    , itWorldModel  = ["models/powerups/health/small_cross.md3", "models/powerups/health/small_sphere.md3"]
    , itIcon        = "icons/iconh_green"
    , itPickupName  = "5 Health"
    , itQuantity    = 5
    , itType        = IT_HEALTH HE_SMALL
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_health"
    , itPickupSound = Just "sound/items/n_health.wav"
    , itWorldModel  = ["models/powerups/health/medium_cross.md3", "models/powerups/health/medium_sphere.md3"]
    , itIcon        = "icons/iconh_yellow"
    , itPickupName  = "25 Health"
    , itQuantity    = 25
    , itType        = IT_HEALTH HE_MEDIUM
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_health_large"
    , itPickupSound = Just "sound/items/l_health.wav"
    , itWorldModel  = ["models/powerups/health/large_cross.md3", "models/powerups/health/large_sphere.md3"]
    , itIcon        = "icons/iconh_red"
    , itPickupName  = "50 Health"
    , itQuantity    = 50
    , itType        = IT_HEALTH HE_LARGE
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_health_mega"
    , itPickupSound = Just "sound/items/m_health.wav"
    , itWorldModel  = ["models/powerups/health/mega_cross.md3", "models/powerups/health/mega_sphere.md3"]
    , itIcon        = "icons/iconh_mega"
    , itPickupName  = "Mega Health"
    , itQuantity    = 100
    , itType        = IT_HEALTH HE_MEGA
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_gauntlet"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/gauntlet/gauntlet.md3"]
    , itIcon        = "icons/iconw_gauntlet"
    , itPickupName  = "Gauntlet"
    , itQuantity    = 0
    , itType        = IT_WEAPON WP_GAUNTLET
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_shotgun"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/shotgun/shotgun.md3"]
    , itIcon        = "icons/iconw_shotgun"
    , itPickupName  = "Shotgun"
    , itQuantity    = 10
    , itType        = IT_WEAPON WP_SHOTGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_machinegun"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/machinegun/machinegun.md3"]
    , itIcon        = "icons/iconw_machinegun"
    , itPickupName  = "Machinegun"
    , itQuantity    = 40
    , itType        = IT_WEAPON WP_MACHINEGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_grenadelauncher"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/grenadel/grenadel.md3"]
    , itIcon        = "icons/iconw_grenade"
    , itPickupName  = "Grenade Launcher"
    , itQuantity    = 10
    , itType        = IT_WEAPON WP_GRENADE_LAUNCHER
    , itSounds      = ["sound/weapons/grenade/hgrenb1a.wav", "sound/weapons/grenade/hgrenb2a.wav"]
    }
  , Item
    { itClassName   = "weapon_rocketlauncher"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/rocketl/rocketl.md3"]
    , itIcon        = "icons/iconw_rocket"
    , itPickupName  = "Rocket Launcher"
    , itQuantity    = 10
    , itType        = IT_WEAPON WP_ROCKET_LAUNCHER
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_lightning"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/lightning/lightning.md3"]
    , itIcon        = "icons/iconw_lightning"
    , itPickupName  = "Lightning Gun"
    , itQuantity    = 100
    , itType        = IT_WEAPON WP_LIGHTNING
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_railgun"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/railgun/railgun.md3"]
    , itIcon        = "icons/iconw_railgun"
    , itPickupName  = "Railgun"
    , itQuantity    = 10
    , itType        = IT_WEAPON WP_RAILGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_plasmagun"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/plasma/plasma.md3"]
    , itIcon        = "icons/iconw_plasma"
    , itPickupName  = "Plasma Gun"
    , itQuantity    = 50
    , itType        = IT_WEAPON WP_PLASMAGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_bfg"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/bfg/bfg.md3"]
    , itIcon        = "icons/iconw_bfg"
    , itPickupName  = "BFG10K"
    , itQuantity    = 20
    , itType        = IT_WEAPON WP_BFG
    , itSounds      = []
    }
  , Item
    { itClassName   = "weapon_grapplinghook"
    , itPickupSound = Just "sound/misc/w_pkup.wav"
    , itWorldModel  = ["models/weapons2/grapple/grapple.md3"]
    , itIcon        = "icons/iconw_grapple"
    , itPickupName  = "Grappling Hook"
    , itQuantity    = 0
    , itType        = IT_WEAPON WP_GRAPPLING_HOOK
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_shells"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/shotgunam.md3"]
    , itIcon        = "icons/icona_shotgun"
    , itPickupName  = "Shells"
    , itQuantity    = 10
    , itType        = IT_AMMO WP_SHOTGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_bullets"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/machinegunam.md3"]
    , itIcon        = "icons/icona_machinegun"
    , itPickupName  = "Bullets"
    , itQuantity    = 50
    , itType        = IT_AMMO WP_MACHINEGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_grenades"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/grenadeam.md3"]
    , itIcon        = "icons/icona_grenade"
    , itPickupName  = "Grenades"
    , itQuantity    = 5
    , itType        = IT_AMMO WP_GRENADE_LAUNCHER
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_cells"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/plasmaam.md3"]
    , itIcon        = "icons/icona_plasma"
    , itPickupName  = "Cells"
    , itQuantity    = 30
    , itType        = IT_AMMO WP_PLASMAGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_lightning"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/lightningam.md3"]
    , itIcon        = "icons/icona_lightning"
    , itPickupName  = "Lightning"
    , itQuantity    = 60
    , itType        = IT_AMMO WP_LIGHTNING
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_rockets"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/rocketam.md3"]
    , itIcon        = "icons/icona_rocket"
    , itPickupName  = "Rockets"
    , itQuantity    = 5
    , itType        = IT_AMMO WP_ROCKET_LAUNCHER
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_slugs"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/railgunam.md3"]
    , itIcon        = "icons/icona_railgun"
    , itPickupName  = "Slugs"
    , itQuantity    = 10
    , itType        = IT_AMMO WP_RAILGUN
    , itSounds      = []
    }
  , Item
    { itClassName   = "ammo_bfg"
    , itPickupSound = Just "sound/misc/am_pkup.wav"
    , itWorldModel  = ["models/powerups/ammo/bfgam.md3"]
    , itIcon        = "icons/icona_bfg"
    , itPickupName  = "Bfg Ammo"
    , itQuantity    = 15
    , itType        = IT_AMMO WP_BFG
    , itSounds      = []
    }
  , Item
    { itClassName   = "holdable_teleporter"
    , itPickupSound = Just "sound/items/holdable.wav"
    , itWorldModel  = ["models/powerups/holdable/teleporter.md3"]
    , itIcon        = "icons/teleporter"
    , itPickupName  = "Personal Teleporter"
    , itQuantity    = 60
    , itType        = IT_HOLDABLE HI_TELEPORTER
    , itSounds      = []
    }
  , Item
    { itClassName   = "holdable_medkit"
    , itPickupSound = Just "sound/items/holdable.wav"
    , itWorldModel  = ["models/powerups/holdable/medkit.md3", "models/powerups/holdable/medkit_sphere.md3"]
    , itIcon        = "icons/medkit"
    , itPickupName  = "Medkit"
    , itQuantity    = 60
    , itType        = IT_HOLDABLE HI_MEDKIT
    , itSounds      = ["sound/items/use_medkit.wav"]
    }
  , Item
    { itClassName   = "item_quad"
    , itPickupSound = Just "sound/items/quaddamage.wav"
    , itWorldModel  = ["models/powerups/instant/quad.md3", "models/powerups/instant/quad_ring.md3"]
    , itIcon        = "icons/quad"
    , itPickupName  = "Quad Damage"
    , itQuantity    = 30
    , itType        = IT_POWERUP PW_QUAD
    , itSounds      = ["sound/items/damage2.wav", "sound/items/damage3.wav"]
    }
  , Item
    { itClassName   = "item_enviro"
    , itPickupSound = Just "sound/items/protect.wav"
    , itWorldModel  = ["models/powerups/instant/enviro.md3", "models/powerups/instant/enviro_ring.md3"]
    , itIcon        = "icons/envirosuit"
    , itPickupName  = "Battle Suit"
    , itQuantity    = 30
    , itType        = IT_POWERUP PW_BATTLESUIT
    , itSounds      = ["sound/items/airout.wav", "sound/items/protect3.wav"]
    }
  , Item
    { itClassName   = "item_haste"
    , itPickupSound = Just "sound/items/haste.wav"
    , itWorldModel  = ["models/powerups/instant/haste.md3", "models/powerups/instant/haste_ring.md3"]
    , itIcon        = "icons/haste"
    , itPickupName  = "Speed"
    , itQuantity    = 30
    , itType        = IT_POWERUP PW_HASTE
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_invis"
    , itPickupSound = Just "sound/items/invisibility.wav"
    , itWorldModel  = ["models/powerups/instant/invis.md3", "models/powerups/instant/invis_ring.md3"]
    , itIcon        = "icons/invis"
    , itPickupName  = "Invisibility"
    , itQuantity    = 30
    , itType        = IT_POWERUP PW_INVIS
    , itSounds      = []
    }
  , Item
    { itClassName   = "item_regen"
    , itPickupSound = Just "sound/items/regeneration.wav"
    , itWorldModel  = ["models/powerups/instant/regen.md3", "models/powerups/instant/regen_ring.md3"]
    , itIcon        = "icons/regen"
    , itPickupName  = "Regeneration"
    , itQuantity    = 30
    , itType        = IT_POWERUP PW_REGEN
    , itSounds      = ["sound/items/regen.wav"]
    }
  , Item
    { itClassName   = "item_flight"
    , itPickupSound = Just "sound/items/flight.wav"
    , itWorldModel  = ["models/powerups/instant/flight.md3", "models/powerups/instant/flight_ring.md3"]
    , itIcon        = "icons/flight"
    , itPickupName  = "Flight"
    , itQuantity    = 60
    , itType        = IT_POWERUP PW_FLIGHT
    , itSounds      = ["sound/items/flight.wav"]
    }
  , Item
    { itClassName   = "team_CTF_redflag"
    , itPickupSound = Nothing
    , itWorldModel  = ["models/flags/r_flag.md3"]
    , itIcon        = "icons/iconf_red1"
    , itPickupName  = "Red Flag"
    , itQuantity    = 0
    , itType        = IT_TEAM PW_REDFLAG
    , itSounds      = []
    }
  , Item
    { itClassName   = "team_CTF_blueflag"
    , itPickupSound = Nothing
    , itWorldModel  = ["models/flags/b_flag.md3"]
    , itIcon        = "icons/iconf_blu1"
    , itPickupName  = "Blue Flag"
    , itQuantity    = 0
    , itType        = IT_TEAM PW_BLUEFLAG
    , itSounds      = []
    }
  ]
  
--weaponModel_Map :: IM.IntMap Item


data WeaponInfo
  = WeaponInfo
  { wiMissileModel :: Maybe String
  , wiFlashModel   :: Maybe String
  , wiHandModel    :: Maybe String
  , wiBarrelModel  :: Maybe String
  , wiType         :: Weapon
  , wiRPM          :: Int         --weapon rate of fire
  }

weaponInfos :: [WeaponInfo]
weaponInfos =
  [ WeaponInfo
    { wiMissileModel = Nothing
    , wiType         = WP_GAUNTLET
    , wiFlashModel   = Nothing
    , wiHandModel    = Nothing
    , wiBarrelModel  = Nothing
    , wiRPM          = 500
    }
  , WeaponInfo
    { wiMissileModel = Nothing
    , wiType         = WP_MACHINEGUN
    , wiFlashModel   = Just "models/weapons2/machinegun/machinegun_flash.md3"
    , wiHandModel    = Just "models/weapons2/machinegun/machinegun_hand.md3"
    , wiBarrelModel  = Just "models/weapons2/machinegun/machinegun_barrel.md3"
    , wiRPM          = 500
    }
  , WeaponInfo
    { wiMissileModel = Nothing
    , wiType         = WP_SHOTGUN
    , wiFlashModel   = Just "models/weapons2/shotgun/shotgun_flash.md3"
    , wiHandModel    = Just "models/weapons2/shotgun/shotgun_hand.md3"
    , wiBarrelModel  = Nothing
    , wiRPM          = 300
    }
  , WeaponInfo
    { wiMissileModel = Just "models/ammo/grenade1.md3"
    , wiType         = WP_GRENADE_LAUNCHER
    , wiFlashModel   = Nothing
    , wiHandModel    = Nothing
    , wiBarrelModel  = Nothing
    , wiRPM          = 100
    }
  , WeaponInfo
    { wiMissileModel = Just "models/ammo/rocket/rocket.md3"
    , wiHandModel    = Just "models/weapons2/rocketl/rocketl_hand.md3"
    , wiFlashModel   = Just "models/weapons2/rocketl/rocketl_flash.md3"
    , wiBarrelModel  = Nothing
    , wiType         = WP_ROCKET_LAUNCHER
    , wiRPM          = 200
    }
  , WeaponInfo
    { wiMissileModel = Nothing
    , wiType         = WP_LIGHTNING
    , wiFlashModel   = Nothing
    , wiHandModel    = Nothing
    , wiBarrelModel  = Nothing
    , wiRPM          = 100
    }
  , WeaponInfo
    { wiMissileModel = Nothing
    , wiType         = WP_RAILGUN
    , wiFlashModel   = Nothing
    , wiHandModel    = Nothing
    , wiBarrelModel  = Nothing
    , wiRPM          = 100
    }
  , WeaponInfo
    { wiMissileModel = Nothing
    , wiType         = WP_PLASMAGUN
    , wiHandModel    = Just "models/weapons2/plasma/plasma_hand.md3"
    , wiFlashModel   = Just "models/weapons2/plasma/plasma_flash.md3"
    , wiBarrelModel  = Nothing
    , wiRPM          = 100
    }
  , WeaponInfo
    { wiMissileModel = Just "models/weaphits/bfg.md3"
    , wiType         = WP_BFG
    , wiFlashModel   = Nothing
    , wiHandModel    = Nothing
    , wiBarrelModel  = Nothing
    , wiRPM          = 100
    }
  , WeaponInfo
    { wiMissileModel = Just "models/ammo/rocket/rocket.md3"
    , wiBarrelModel  = Nothing
    , wiHandModel    = Nothing
    , wiType         = WP_GRAPPLING_HOOK
    , wiFlashModel   = Nothing
    , wiRPM          = 100
    }
  ]
