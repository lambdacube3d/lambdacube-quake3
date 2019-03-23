{-# LANGUAGE RecordWildCards #-}
module Camera where

import Control.Applicative
import Data.Vect
import qualified Data.Vector as V
import FRP.Elerea.Param

import GameEngine.Collision
import GameEngine.Data.BSP

userCamera :: ([Int] -> Vec3 -> Vec3) -> BSPLevel -> Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool, Bool)
           -> SignalGen Float (Signal (Vec3, Vec3, Vec3, [Int]))
userCamera camTr bsp p mposs keyss = fmap (\(pos,target,up,i,_) -> (pos,target,up,i)) <$> transfer2 (p,zero,zero,[],(0,0,0)) calcCam mposs keyss
  where
    clamp' minimal maximal = min maximal . max minimal
    up = Vec3 0 0 1 --felfelé mutató vektor
    gravity = 1000
    jumpSpeed0 = 300
    height = 42
    calcCam dt (dmx,dmy) {-az egér elmozdulása-} (left,forward,backward,right,turbo,jump) (prevCamPos, _ , _,bIdx0,(mx,my,fallingSpeed)) =
      let nil c n = if c then n else zero
          movedCam  = nil left (st &* (-movementScalar)) &+ nil forward (forwardOnlyXY &* movementScalar) &+ nil backward (forwardOnlyXY &* (-movementScalar)) &+ nil right (st &* movementScalar) &+ prevCamPos
          forwardOnlyXY = up &^ st
          turboScalar = if turbo then 500 else 200
          movementScalar = turboScalar * realToFrac dt
          newmx = mx - dmx / 100
          newmy = clamp' 0.1 3.1 (my + dmy / 100)
          fw = let sinNewmy = sin newmy in Vec3 (cos newmx * sinNewmy) (sin newmx * sinNewmy) (cos  newmy) --gömbi koordináták a parametrikus alakból, https://hu.wikipedia.org/wiki/G%C3%B6mb
          st = normalize $ fw &^ up
          jumpSpeed' = if jump then jumpSpeed0 else 0
          headBob = Vec3 0 0 0
          fallingVec = Vec3 0 0 (fallingSpeed * dt)
          fallCamPos = movedCam &+ fallingVec
          (camPos,bIdx'1) = case traceRay {-Sphere (height / 2.3)-} bsp prevCamPos fallCamPos of --régi pozícióból az új felé indított sugár ütközik e valamibe? ha nem, akkor léphetünk oda
            Nothing -> (fallCamPos,[])
            Just (hit,TraceHit{..}) -> (prevCamPos &+ fallingVec,outputBrushIndex)
      in case traceRay bsp camPos (camPos &- Vec3 0 0 (height+1)) of
          Just (hit,TraceHit{..}) -> --a kamera megáll az alatta lévő akadály miatt
                          let finalCamPos = camTr bIdx0 $ hit &+ Vec3 0 0 height --a kamera pozíciója a metszéspont felett adott magasságban lesz
                          in (finalCamPos, finalCamPos &+ fw, up, outputBrushIndex ++ bIdx'1, (newmx, newmy, jumpSpeed'))
          Nothing -> (camPos, camPos &+ fw, up, bIdx'1, (newmx, newmy, fallingSpeed - dt*gravity + jumpSpeed'))


recordSignalSamples :: Signal Bool -> Signal Bool -> Signal a -> SignalGen p (Signal [a])
recordSignalSamples = transfer3 [] record
  where
    record _ setWaypoint clearWaypoints input history
        | clearWaypoints = [] 
        | setWaypoint    = input:history 
        | otherwise      = history

playbackCamera :: Signal Bool -> Signal Bool -> Signal Float -> Signal [(Vec3, Vec3)] -> SignalGen Float (Signal (Maybe (Vec3, Vec3, Vec3)))
playbackCamera play stop speed recording = do
    let noPath = (V.empty, V.empty)
        trackPath _ play stop waypoints path 
            | stop      = noPath 
            | play      = mkPath waypoints 
            | otherwise = path
        mkPath waypoints = (camPath, targetPath)
          where
            waypoints' = reverse waypoints
            camPath = extendPath (V.fromList (map fst waypoints'))
            targetPath = extendPath (V.fromList (map snd waypoints'))
        
        stepCamera dtime (camPath, _targetPath) speed t
            | V.length camPath < 4 = 0
            | otherwise            = if t' > tmax - 0.05 then t' - tmax else t'
          where
            t' = proceedOnPath camPath 50 t (dtime * speed)
            tmax = fromIntegral (V.length camPath - 3)
    
    path <- transfer3 noPath trackPath play stop recording
    param <- transfer2 0 stepCamera path speed
    return $ do
        (camPath, targetPath) <- path
        t <- param
        return $ if V.length camPath < 4 then Nothing else Just (samplePath camPath t, samplePath targetPath t, Vec3 0 0 1)

extendPath :: V.Vector Vec3 -> V.Vector Vec3
extendPath ps = V.snoc (V.cons (2 *& ps V.! 0 &- ps V.! 1) ps) (2 *& ps V.! l &- ps V.! (l-1))
  where
    l = V.length ps - 1

proceedOnPath :: V.Vector Vec3 -> Int -> Float -> Float -> Float
proceedOnPath ps prec t d = go t (samplePath ps t) 0
  where
    tmax = fromIntegral (V.length ps - 3)
    go t p s
        | s > d     = t
        | t' > tmax = t
        | otherwise = go t' p' (s + len (p' &- p))
      where
        t' = t + d / (len grad * fromIntegral prec)
        p' = samplePath ps t'
        (i, f) = properFraction t
        grad = spline' (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
{-    
    iterate step t !! prec
  where
    step t = t + d / (len s * fromIntegral prec)
      where
        (i, f) = properFraction t
        s = spline' (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
-}

{-

 f(t0) = p0
 f(t0+t) = p0+d

 t = ?

 f'(t0) = p'0
 f(t0+1) ~= p0+p'0 
 f(t0+x) ~= p0+d/prec  ->  x = d/(prec*p'0)

-}

samplePath :: V.Vector Vec3 -> Float -> Vec3
samplePath ps t = spline (ps V.! i) (ps V.! (i+1)) (ps V.! (i+2)) (ps V.! (i+3)) f
  where
    (i, f) = properFraction t

spline :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
spline p0 p1 p2 p3 t = 0.5 *& (2*&p1 &+ (p2 &- p0) &* t &+ (2*&p0 &- 5*&p1 &+ 4*&p2 &- p3) &* t^2 &+ (neg p0 &+ 3*&p1 &- 3*&p2 &+ p3) &* t^3)

spline' :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
spline' p0 p1 p2 p3 t = 0.5 *& (p2 &- p0 &+ (4*&p0 &- 10*&p1 &+ 8*&p2 &- 2*&p3) &* t &+ ((-3)*&p0 &+ 9*&p1 &- 8*&p2 &+ 3*&p3) &* t^2)
