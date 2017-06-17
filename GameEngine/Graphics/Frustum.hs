module GameEngine.Graphics.Frustum where

import Data.List
import Data.Vect.Float
import Data.Vect.Float.Instances

data Frustum
    = Frustum
    { frPlanes  :: [(Vec3, Float)]
    , ntl       :: Vec3
    , ntr       :: Vec3
    , nbl       :: Vec3
    , nbr       :: Vec3
    , ftl       :: Vec3
    , ftr       :: Vec3
    , fbl       :: Vec3
    , fbr       :: Vec3
    } deriving Show

pointInFrustum :: Vec3 -> Frustum -> Bool
pointInFrustum p fr = foldl' (\b (n,d) -> b && d + n `dotprod` p >= 0) True $ frPlanes fr

sphereInFrustum :: Vec3 -> Float -> Frustum -> Bool
sphereInFrustum p r fr = foldl' (\b (n,d) -> b && d + n `dotprod` p >= (-r)) True $ frPlanes fr

boxInFrustum :: Vec3 -> Vec3 -> Frustum -> Bool
boxInFrustum pp pn fr = foldl' (\b (n,d) -> b && d + n `dotprod` (g pp pn n) >= 0) True $ frPlanes fr
  where
    g (Vec3 px py pz) (Vec3 nx ny nz) n = Vec3 (fx px nx) (fy py ny) (fz pz nz)
      where
        Vec3 x y z = n
        [fx,fy,fz] = map (\a -> if a > 0 then max else min) [x,y,z]

frustum :: Float -> Float -> Float -> Float -> Vec3 -> Vec3 -> Vec3 -> Frustum
frustum viewAngle aspectRatio nearDistance farDistance position lookat up =
  Frustum [ (pl ntr ntl ftl)
          , (pl nbl nbr fbr)
          , (pl ntl nbl fbl)
          , (pl nbr ntr fbr)
          , (pl ntl ntr nbr)
          , (pl ftr ftl fbl)
          ] ntl ntr nbl nbr ftl ftr fbl fbr
  where
    pl a b c = (n,d)
      where
        n = normalize $ (c - b) `crossprod` (a - b)
        d = -(n `dotprod` b)
    m a v = scalarMul a v
    ang2rad = pi / 180
    tang    = tan $ viewAngle * ang2rad * 0.5
    nh  = nearDistance * tang
    nw  = nh * aspectRatio
    fh  = farDistance * tang
    fw  = fh * aspectRatio
    z   = normalize $ position - lookat
    x   = normalize $ up `crossprod` z
    y   = z `crossprod` x

    nc  = position - m nearDistance z
    fc  = position - m farDistance z

    ntl = nc + m nh y - m nw x
    ntr = nc + m nh y + m nw x
    nbl = nc - m nh y - m nw x
    nbr = nc - m nh y + m nw x

    ftl = fc + m fh y - m fw x
    ftr = fc + m fh y + m fw x
    fbl = fc - m fh y - m fw x
    fbr = fc - m fh y + m fw x
