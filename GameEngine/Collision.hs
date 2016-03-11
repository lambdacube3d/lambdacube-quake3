{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns #-}
module GameEngine.Collision
  ( TraceHit(..)
  , traceRay
  , traceSphere
  , traceBox
  ) where

{-
  Quake 3 BSP Collision Detection
  http://openzone.googlecode.com/git-history/f73bb8dfe8e6a16c13d39aba1c8f6537ee263d07/doc/Quake3BSP.html
  https://github.com/JodiTheTigger/MessyBsp/blob/master/docs/section6.txt
-}

-- TODO - for box-box, etc collision see: code/qcommon/cm_trace.c

import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Vect.Float
import Data.Vect.Float.Instances
import Data.Bits ((.&.))
import GameEngine.BSP

data TraceHit
  = TraceHit
  { outputFraction    :: !Float
  , outputStartsOut   :: !Bool
  , outputAllSolid    :: !Bool
  , outputBrushIndex  :: ![Int]
  }
  deriving Show

traceRay :: BSPLevel -> Vec3 -> Vec3 -> Maybe (Vec3,TraceHit)
traceRay = traceHit TraceRay

traceSphere :: Float -> BSPLevel -> Vec3 -> Vec3 -> Maybe (Vec3,TraceHit)
traceSphere radius = traceHit $ TraceSphere radius

traceBox :: Vec3 -> Vec3 -> BSPLevel -> Vec3 -> Vec3 -> Maybe (Vec3,TraceHit)
traceBox mins@(Vec3 x1 y1 z1) maxs@(Vec3 x2 y2 z2)
  | mins == zero && maxs == zero = traceRay
  | otherwise = traceHit $ TraceBox mins maxs $ Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
  where f a b = max (abs a) (abs b)

data TraceType
  = TraceRay
  | TraceSphere Float           -- radius
  | TraceBox    Vec3 Vec3 Vec3  -- mins maxs extends

traceHit :: TraceType -> BSPLevel -> Vec3 -> Vec3 -> Maybe (Vec3,TraceHit)
traceHit traceType bsp inputStart inputEnd
  | outputFraction == 1 = Nothing 
  | otherwise = Just (outputEnd,th)
 where
    th@TraceHit{..} = checkNode True traceType bsp inputStart inputEnd 0 0 1 inputStart inputEnd
    outputEnd = inputStart + outputFraction *& (inputEnd - inputStart)

{-
monoid (min,and,or)
float outputFraction;    -- min
boolean outputStartsOut; -- mappend: and
boolean outputAllSolid;  -- mappend: or
-}

instance Monoid TraceHit where
  mempty = TraceHit 1 True False []
  (TraceHit a1 b1 c1 d1) `mappend` (TraceHit a2 b2 c2 d2) = TraceHit (min a1 a2) (b1 && b2) (c1 || c2) (d1 `mappend` d2)

epsilon = 1/32
clamp = max 0 . min 1

checkNode noCull traceType bsp@BSPLevel{..} inputStart inputEnd nodeIndex startFraction endFraction start end
  -- leaf
  | nodeIndex < 0 || noCull =
      let Leaf{..} = blLeaves ! (-(nodeIndex + 1))
          leafBrushes = V.take lfNumLeafBrushes $ V.drop lfFirstLeafBrush blLeafBrushes
      in mconcat [ checkBrush traceType bsp inputStart inputEnd brushIndex brush
                 | brushIndex <- if noCull then [0..V.length blBrushes-1] else V.toList $ leafBrushes
                 , let brush@Brush{..} = blBrushes ! brushIndex
                 , brNumSides > 0
                 , shContentFlags (blShaders ! brShaderNum) .&. 1 == 1
                 ]
  -- node
  | startDistance >= offset && endDistance >= offset = checkNode noCull traceType bsp inputStart inputEnd (fst ndChildren) startFraction endFraction start end
  | startDistance < -offset && endDistance < -offset = checkNode noCull traceType bsp inputStart inputEnd (snd ndChildren) startFraction endFraction start end
  | otherwise =
      let inverseDistance = 1 / (startDistance - endDistance)
          (side,clamp -> fraction1,clamp -> fraction2)
            | startDistance < endDistance = (True, (startDistance - offset + epsilon) * inverseDistance,(startDistance + offset + epsilon) * inverseDistance)
            | endDistance < startDistance = (False,(startDistance + offset + epsilon) * inverseDistance,(startDistance - offset - epsilon) * inverseDistance)
            | otherwise = (False,1,0)
          middleFraction1 = startFraction + (endFraction - startFraction) * fraction1
          middleFraction2 = startFraction + (endFraction - startFraction) * fraction2
          middle1 = start + fraction1 *& (end - start)
          middle2 = start + fraction2 *& (end - start)
          selectChildren b = if b then snd ndChildren else fst ndChildren
      in checkNode noCull traceType bsp inputStart inputEnd (selectChildren side) startFraction middleFraction1 start middle1 `mappend`
         checkNode noCull traceType bsp inputStart inputEnd (selectChildren $ not side) middleFraction2 endFraction middle2 end
 where
  Node{..}  = blNodes ! nodeIndex
  Plane{..} = blPlanes ! ndPlaneNum
  startDistance = start `dotprod` plNormal - plDist
  endDistance = end `dotprod` plNormal - plDist
  !offset = case traceType of
    TraceRay -> 0
    TraceSphere radius -> radius
    TraceBox _ _ (Vec3 x y z) -> let Vec3 px py pz = plNormal in abs (x * px) + abs (y * py) + abs (z * pz)

checkBrush traceType BSPLevel{..} inputStart inputEnd brushIndex Brush{..} =
  let brushPlanes = fmap ((blPlanes !) . bsPlaneNum) $ V.take brNumSides . V.drop brFirstSide $ blBrushSides
      startEndDistances = case traceType of
        TraceRay -> [(inputStart `dotprod` plNormal - plDist, inputEnd `dotprod` plNormal - plDist) | Plane{..} <- V.toList brushPlanes]
        TraceSphere radius -> [(inputStart `dotprod` plNormal - (plDist + radius), inputEnd `dotprod` plNormal - (plDist + radius)) | Plane{..} <- V.toList brushPlanes]
        TraceBox (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) _ ->
          [ ((inputStart &+ offset) `dotprod` plNormal - plDist, (inputEnd &+ offset) `dotprod` plNormal - plDist)
          | Plane{..} <- V.toList brushPlanes
          , let Vec3 px py pz = plNormal
                offset = Vec3 (if px < 0 then x2 else x1)
                              (if py < 0 then y2 else y1)
                              (if pz < 0 then z2 else z1)
          ]

      eval p [] = Just p
      eval p@(startFraction,endFraction,startsOut,endsOut) ((startDistance,endDistance):xs)
        | startDistance > 0 && endDistance > 0 = Nothing -- both are in front of the plane, its outside of this brush
        | startDistance <= 0 && endDistance <= 0 = eval p xs
        | startDistance > endDistance = let fraction = (startDistance - epsilon) / (startDistance - endDistance)
                                        in  eval (max fraction startFraction,endFraction,startsOut || startDistance > 0,endsOut || endDistance > 0) xs
        | otherwise = let fraction = (startDistance + epsilon) / (startDistance - endDistance)
                      in  eval (startFraction, min fraction endFraction,startsOut || startDistance > 0,endsOut || endDistance > 0) xs
  in case eval (-1,1,False,False) startEndDistances of
      Just (startFraction,endFraction,startsOut,endsOut)
        | startsOut == False    -> TraceHit 1 False (not endsOut) mempty
        | startFraction < endFraction
          && startFraction > -1 -> TraceHit (max startFraction 0) True False [brushIndex]
      _ -> mempty
