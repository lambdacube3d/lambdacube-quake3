module GameEngine.Graphics.Culling
  ( cullSurfaces
  ) where

import Control.Monad
import Data.Bits
import Data.Vect.Float
import Data.Vect.Float.Instances
import qualified Data.Vector as V
import LambdaCube.GL

import GameEngine.Data.BSP
import GameEngine.Graphics.Frustum

isClusterVisible :: BSPLevel -> Int -> Int -> Bool
isClusterVisible bl a b
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (b .&. 7)))
    | otherwise = True
  where
    Visibility nvecs szvecs vecs = blVisibility bl
    i = a * szvecs + (shiftR b 3)
    visSet = vecs V.! i

findLeafIdx :: BSPLevel -> Vec3 -> Int -> Int
findLeafIdx bl camPos i
    | i >= 0 = if dist >= 0 then findLeafIdx bl camPos f else findLeafIdx bl camPos b
    | otherwise = (-i) - 1
  where 
    node    = blNodes bl V.! i
    (f,b)   = ndChildren node 
    plane   = blPlanes bl V.! ndPlaneNum node
    dist    = plNormal plane `dotprod` camPos - plDist plane

cullSurfaces :: BSPLevel -> Vec3 -> Frustum -> V.Vector [Object] -> IO ()
cullSurfaces bsp cam frust objs = case leafIdx < 0 || leafIdx >= V.length leaves of
    True    -> {-trace "findLeafIdx error" $ -}V.forM_ objs $ \objList -> forM_ objList $ \obj -> enableObject obj True
    False   -> {-trace ("findLeafIdx ok " ++ show leafIdx ++ " " ++ show camCluster) -}surfaceMask
  where
    leafIdx = findLeafIdx bsp cam 0
    leaves = blLeaves bsp
    camCluster = lfCluster $ leaves V.! leafIdx
    visibleLeafs = V.filter (\a -> (isClusterVisible bsp camCluster $ lfCluster a) && inFrustum a) leaves
    surfaceMask = do
        let leafSurfaces = blLeafSurfaces bsp
        V.forM_ objs $ \objList -> forM_ objList $ \obj -> enableObject obj False
        V.forM_ visibleLeafs $ \l ->
            V.forM_ (V.slice (lfFirstLeafSurface l) (lfNumLeafSurfaces l) leafSurfaces) $ \i ->
                forM_ (objs V.! i) $ \obj -> enableObject obj True
    inFrustum a = boxInFrustum (lfMaxs a) (lfMins a) frust
