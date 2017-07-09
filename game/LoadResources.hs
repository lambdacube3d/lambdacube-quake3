module LoadResources where

import GameEngine.Scene (Resource)
import World

worldResources :: World -> [Resource]
worldResources _ = []
{-
  (renderables,Last mcamera) = execWriter . forM_ (w^.wEntities) $ \case
    EBullet b   -> [R_MD3 "models/ammo/rocket/rocket.md3"]
    EWeapon a   -> add [R_MD3 model | model <- itWorldModel (itemMap ! (IT_WEAPON $ a^.wType))]
    EAmmo a     -> add [R_MD3 model | model <- itWorldModel (itemMap ! (IT_AMMO $ a^.aType))]
    EArmor a    -> add [R_MD3 model | model <- itWorldModel (itemMap ! (IT_ARMOR $ a^.rType))]
    EHealth a   -> add [R_MD3 model | model <- itWorldModel (itemMap ! (IT_HEALTH $ a^.hType))]
    EHoldable h -> add [R_MD3 model | model <- itWorldModel (itemMap ! (IT_HOLDABLE $ h^.hoType))]
    EPowerup p  -> add [R_MD3 model | model <- itWorldModel (itemMap ! (IT_POWERUP $ p^.puType))]
-}
