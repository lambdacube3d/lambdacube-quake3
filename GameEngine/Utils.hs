module GameEngine.Utils where

import Data.Time.Clock
import Text.Printf
import Data.Bits
import Data.Vect.Float
import Data.Vect.Float.Instances
import Data.Vect.Float.GramSchmidt
import Data.Vect.Float.Util.Quaternion
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.FilePath
import LambdaCube.Linear
import LambdaCube.Mesh

lc_q3_cache = ".lc_q3.cache" -- local cache: generated files, compiled pipelines are stored here

-- simple meshes
sphere :: V4 Float -> Int -> Float -> Mesh
sphere color n radius = Mesh
    { mAttributes = Map.fromList [("position", A_V3F vertices), ("normal", A_V3F normals), ("color", A_V4F $ V.replicate (V.length vertices) color)]
    , mPrimitive = P_TrianglesI indices
    }
  where
    m = pi / fromIntegral n
    vertices = V.map (\(V3 x y z) -> V3 (radius * x) (radius * y) (radius * z)) normals
    normals = V.fromList [V3 (sin a * cos b) (cos a) (sin a * sin b) | i <- [0..n], j <- [0..2 * n - 1],
                          let a = fromIntegral i * m, let b = fromIntegral j * m]
    indices = V.fromList $ concat [[ix i j, ix i' j, ix i' j', ix i' j', ix i j', ix i j] | i <- [0..n - 1], j <- [0..2 * n - 1],
                                   let i' = i + 1, let j' = (j + 1) `mod` (2 * n)]
    ix i j = fromIntegral (i * 2 * n + j)

bbox :: V4 Float -> Vec3 -> Vec3 -> Mesh
bbox color (Vec3 minX minY minZ) (Vec3 maxX maxY maxZ) = Mesh
    { mAttributes = Map.fromList [("position", A_V3F vertices), ("color", A_V4F $ V.replicate (V.length vertices) color)]
    , mPrimitive = P_Triangles
    }
  where
    quads = [[6, 2, 3, 7], [5, 1, 0, 4], [7, 3, 1, 5], [4, 0, 2, 6], [3, 2, 0, 1], [6, 7, 5, 4]]
    indices = V.fromList $ concat [[a, b, c, c, d, a] | [d, c, b, a] <- quads]
    vertices = V.backpermute (V.generate 8 mkVertex) indices

    mkVertex n = V3 x y z
      where
        x = if testBit n 2 then maxX else minX
        y = if testBit n 1 then maxY else minY
        z = if testBit n 0 then maxZ else minZ

-- matrix functions

ortho :: Float -- ^ Left
      -> Float -- ^ Right
      -> Float -- ^ Bottom
      -> Float -- ^ Top
      -> Float -- ^ Near
      -> Float -- ^ Far
      -> Mat4
ortho l r b t n f =
  Mat4 (Vec4 (-2*x) 0      0     ((r+l)*x))
       (Vec4 0      (-2*y) 0     ((t+b)*y))
       (Vec4 0      0      (2*z) ((f+n)*z))
       (Vec4 0      0      0     1)
  where x = recip(l-r)
        y = recip(b-t)
        z = recip(n-f)

-- | Perspective transformation matrix in row major order.
perspective :: Float  -- ^ Near plane clipping distance (always positive).
            -> Float  -- ^ Far plane clipping distance (always positive).
            -> Float  -- ^ Field of view of the y axis, in radians.
            -> Float  -- ^ Aspect ratio, i.e. screen's width\/height.
            -> Mat4
perspective n f fovy aspect = transpose $
    Mat4 (Vec4 (2*n/(r-l))       0       (-(r+l)/(r-l))        0)
         (Vec4     0        (2*n/(t-b))  ((t+b)/(t-b))         0)
         (Vec4     0             0       (-(f+n)/(f-n))  (-2*f*n/(f-n)))
         (Vec4     0             0            (-1)             0)
  where
    t = n*tan(fovy/2)
    b = -t
    r = aspect*t
    l = -r

-- | Camera transformation matrix.
lookat :: Vec3   -- ^ Camera position.
       -> Vec3   -- ^ Target position.
       -> Vec3   -- ^ Upward direction.
       -> Proj4
lookat pos target up = translateBefore4 (neg pos) (orthogonal $ toOrthoUnsafe r)
  where
    w = normalize $ pos &- target
    u = normalize $ up &^ w
    v = w &^ u
    r = transpose $ Mat3 u v w

lookRotation :: Vec3   -- ^ Target position.
             -> Vec3   -- ^ Upward direction.
             -> UnitQuaternion
lookRotation lookAt upDirection = unsafeToU $ Vec4 x y z w where
  (forward,up) = gramSchmidtNormalize (lookAt,upDirection)
  right = up `crossprod` forward

  w = sqrt (1 + m00 + m11 + m22) * 0.5
  w4_recip = 1 / (4 * w)
  x = (m21 - m12) * w4_recip
  y = (m02 - m20) * w4_recip
  z = (m10 - m01) * w4_recip

  m00 = _1 right
  m01 = _1 up
  m02 = _1 forward
  m10 = _2 right
  m11 = _2 up
  m12 = _2 forward
  m20 = _3 right
  m21 = _3 up
  m22 = _3 forward

toWorldMatrix :: Vec3 -> UnitQuaternion -> Float -> Proj4
toWorldMatrix position orientation scale = translateAfter4 position . scaleAfterProj4 (Vec3 scale scale scale) . orthogonal $ rightOrthoU orientation

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

vec3ToV3F :: Vec3 -> V3F
vec3ToV3F (Vec3 x y z) = V3 x y z

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)

-- simple benchamrking functions

showTime delta
    | t > 1e-1  = printf "%.3fs" t
    | t > 1e-3  = printf "%.1fms" (t/1e-3)
    | otherwise = printf "%.0fus" (t/1e-6)
  where
    t = realToFrac delta :: Double

timeDiff m = (\s x e -> (diffUTCTime e s, x))
  <$> getCurrentTime
  <*> m
  <*> getCurrentTime

printTimeDiff message m = do
  (t,r) <- timeDiff m
  putStr message
  putStrLn $ showTime t
  return r

setNub :: Ord a => [a] -> [a]
setNub = Set.toList . Set.fromList

sphere_UV_toQuat :: Vec3 -> UnitQuaternion
sphere_UV_toQuat (Vec3 u _ v) = v_rot .*. u_rot 
 where 
  u_rot = rotU' (toNormalUnsafe $ Vec3 0 0 1) u
  axis = u_rot `actU` Vec3 0 1 0
  v_rot = rotU axis (v - pi / 2)
  
rotationBetween :: Vec3 -> Vec3 -> UnitQuaternion
rotationBetween u v = rotU (crossprod u v) (acos $ dotprod (normalize u) (normalize v))

