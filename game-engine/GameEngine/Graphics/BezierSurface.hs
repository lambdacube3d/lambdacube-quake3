module GameEngine.Graphics.BezierSurface
  ( tessellatePatch
  ) where

import Control.Monad
import Data.List (foldl')
import Data.Vect.Float hiding (Vector)
import Data.Vect.Float.Instances
import Data.Vector (Vector,(!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import GameEngine.Data.BSP

{-
  See more:
    http://graphics.cs.brown.edu/games/quake/quake3.html#RenderPatch
-}
tessellate :: Vector DrawVertex -> Int -> (Vector DrawVertex,Vector Int)
tessellate controls level = (v,stripsI)
  where
    plus (DrawVertex p1 d1 l1 n1 c1) (DrawVertex p2 d2 l2 n2 c2) = DrawVertex (p1 + p2) (d1 + d2) (l1 + l2) (n1 + n2) (c1 + c2)
    mult (DrawVertex p d l n c) f = DrawVertex (p &* f) (d &* f) (l &* f) (n &* f) (c &* f)
    mix a c0 c1 c2 = let b = 1 - a in (c0 `mult` (b * b)) `plus` (c1 `mult` (2 * b * a)) `plus` (c2 `mult` (a * a))
    l1 = level + 1
    v = V.create $ do
        vertex <- MV.new (l1*l1)
        forM_ [0..level] $ \i -> let a = fromIntegral i / fromIntegral level in MV.write vertex i $ mix a (controls ! 0) (controls ! 3) (controls ! 6)
        forM_ [1..level] $ \i -> do
            let a = fromIntegral i / fromIntegral level
                c0 = mix a (controls ! 0) (controls ! 1) (controls ! 2)
                c1 = mix a (controls ! 3) (controls ! 4) (controls ! 5)
                c2 = mix a (controls ! 6) (controls ! 7) (controls ! 8)
            forM_ [0..level] $ \j -> let a' = fromIntegral j / fromIntegral level in MV.write vertex (i * l1 + j) $ mix a' c0 c1 c2
        return vertex
    -- merge triangle strips using degenerate triangles
    idx row col2 | col2 `mod` 2 == 1 = (row + 1) * l1 + col2 `div` 2
                 | otherwise         = row * l1 + col2 `div` 2
    strips = [V.generate (l1*2) (idx row) | row <- [0..level-1]]
    separate (a:b:c:xs) = a:b:c:separate (b:c:xs)
    separate [] = []
    trisI   = V.concat [V.fromList $ separate $ V.toList s | s <- strips]
    stripsI = V.concat [V.concat [h,s,l] | s <- strips  -- concatenated triangle strips using degenerated triangles
                       , let h = V.singleton $ V.head s -- degenerate triangles will be shown in line polygon mode
                       , let l = V.singleton $ V.last s
                       ]

tessellatePatch :: V.Vector DrawVertex -> Surface -> Int -> (V.Vector DrawVertex,V.Vector Int)
tessellatePatch drawV sf level = (V.concat vl,V.concat il)
  where
    (w,h)   = srPatchSize sf
    gridF :: [DrawVertex] -> [[DrawVertex]]
    gridF l = case splitAt w l of
        (x,[])  -> [x]
        (x,xs)  -> x:gridF xs
    grid        = gridF $ V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) drawV
    controls    = [V.fromList $ concat [take 3 $ drop x l | l <- lines] | x <- [0,2..w-3], y <- [0,2..h-3], let lines = take 3 $ drop y grid]
    patches     = [tessellate c level | c <- controls]
    (vl,il)     = unzip $ reverse $ snd $ foldl' (\(o,l) (v,i) -> (o+V.length v, (v,V.map (+o) i):l)) (0,[]) patches
