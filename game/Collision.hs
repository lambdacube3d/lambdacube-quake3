{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module Collision where

import Data.Maybe
import Data.List
import Data.Vect
import qualified Data.Vector as V
import Control.Monad.State
import Lens.Micro.Platform
--import qualified Data.KdMap.Static as KDT
import GameEngine.RenderSystem
import GameEngine.Data.MD3
import GameEngine.Collision
import Entities
import Monads

getCollisions :: RenderSystem -> [Entity] -> [(Int,Int)]
getCollisions engine entities = result where
  radius = 49
  distanceSquare = radius ^ 2
  result = [(aId,bId) | (aId,a):ents <- tails $ zip [0..] entities, (bId,b) <- ents, collide a b]
  collide a b = fromMaybe False $ do
    p1 <- getPosition a
    p2 <- getPosition b
    return $ normsqr (p1 &- p2) <= distanceSquare
  {-
    - build kd-tree with collision shape and entity id
    - query entities from kd-tree
  -}

  getPosition :: Entity -> Maybe Vec3
  getPosition = \case
    EPlayer a   -> Just (a^.pPosition)
    EBullet b   -> Just (b^.bPosition) -- "models/ammo/rocket/rocket.md3"
    EWeapon a   -> Just (a^.wPosition) -- "models/weapons2/shotgun/shotgun.md3"
    EAmmo a     -> Just (a^.aPosition) -- "models/powerups/ammo/shotgunam.md3"
    EArmor a    -> Just (a^.rPosition) -- "models/powerups/armor/armor_red.md3"
    EHealth a   -> Just (a^.hPosition) -- "models/powerups/health/medium_cross.md3"
    ETeleport a -> Just (a^.tPosition)
    EHoldable a -> Just (a^.hoPosition)
    EPowerup a  -> Just (a^.puPosition)
    _ -> Nothing

  
  
getModel :: Entity -> EntityM object MD3Model
getModel _ = undefined

data IntersectionData = IntersectionData
 {
   _tmin  :: Float
 , _tmax  :: Float
 , _tymin :: Float
 , _tymax :: Float
 , _tzmin :: Float
 , _tzmax :: Float
 }
 
makeLenses ''IntersectionData


swapField a b a' b' = do
 aval <- use a
 bval <- use b
 when (aval > bval) $ do
  a' .= bval
  b' .= aval 

 
--source: https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-box-intersection
rayIntersectsAABB :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> (Bool, (Float, Float))
rayIntersectsAABB 
 orig@(Vec3 ox oy oz) 
 dir @(Vec3 dx dy dz) 
 minb@(Vec3 mix miy miz) 
 maxb@(Vec3 max may maz) = fmap (\idata -> (_tmin idata, _tmax idata)) $ flip runState (IntersectionData {}) $ do
  tmin .= (mix - ox) / dx
  tmax .= (max - ox) / dx
  
  swapField tmin tmax tmin tmax
   
  tymin .= (miy - oy) / dy
  tymax .= (may - oy) / dy
  
  swapField tymin tymax tymin tymax
  
  conditionOne <- do
   tmin'  <- use tmin
   tmax'  <- use tmax
   tymin' <- use tymin
   tymax' <- use tymax
   return ((tmin' > tymax') || (tymin' > tmax'))
  
  idata <- get
  when(_tymin idata > _tmin idata) $ do
   tmin .= _tymin idata
   
  when(_tymax idata < _tmax idata) $ do
   tmax .= _tymax idata
   
  tzmin .= (miz - oz) / dz
  tzmax .= (maz - oz) / dz
  
  swapField tzmin tzmax tzmin tzmax
   
  conditionTwo <- do
   tmin'  <- use tmin
   tmax'  <- use tmax
   tzmin' <- use tzmin
   tzmax' <- use tzmax
   return ((tmin' > tzmax') || (tzmin' > tmax'))
  
  use tzmin >>= \tzmin -> tmin %= Prelude.max tzmin
  
  use tzmax >>= \tzmax -> tmax %= Prelude.min tzmax
  
  swapField tmin tmax tmin tmax
  
  return . not $ conditionOne || conditionTwo
  

{-
 Ray and AABB collision test (the box can be transformed with arbitrary matrix)
 let M = the transformation of the AABB
 let R = ray
 
 R intersects AABB transformed with M if:
  M^-1 * R intersects AABB at Q
  Intersection point: M * Q
-}

rayIntersectsBox :: Vec3 -> Vec3 -> Mat4 -> Vec3 -> Vec3 -> Bool
rayIntersectsBox rayOrigin rayDir transformation boxMin boxMax = undefined

data RayHit = RayHit 
 {
  fraction :: Float
 }
    
getEntitiesIntersectingRay :: V.Vector Entity -> Vec3 -> Vec3 -> [(RayHit, Entity)]
getEntitiesIntersectingRay ents pos dir = undefined
 where models = V.map getModel ents 