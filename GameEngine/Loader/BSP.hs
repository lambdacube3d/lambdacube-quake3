{-# LANGUAGE OverloadedStrings #-}
module GameEngine.Loader.BSP
  ( readBSP
  , loadBSP
  ) where

import Data.Char
import Data.Int
import Data.Word
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import Control.Monad

import GameEngine.Data.BSP

{-
Information: http://graphics.stanford.edu/~kekoa/q3/

Data types

Quake 3 BSP files contains only four basic data types. They are:

Type        Description
ubyte       unsigned byte
int         4-byte integer, little-endian
float       4-byte IEEE float, little-endian
string[n]   string of n ASCII bytes, not necessarily null-terminated

All data in a BSP file is organized into records composed of these four data types.
-}

-- http://www.mralligator.com/q3/

getString   = fmap (SB8.takeWhile (/= '\0')) . getByteString

getWord     = getWord32le

getUByte    = B.get :: Get Word8
getUByte2   = B.get :: Get (Word8,Word8)
getUByte3   = B.get :: Get (Word8,Word8,Word8)

getFloat    = getFloat32le

getVec2     = Vec2 <$> getFloat <*> getFloat
getVec3     = Vec3 <$> getFloat <*> getFloat <*> getFloat
getVec2i    = (\x y -> Vec2 (fromIntegral x) (fromIntegral y)) <$> getInt <*> getInt
getVec3i    = (\x y z -> Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)) <$> getInt <*> getInt <*> getInt

getVec4RGBA = (\r g b a -> Vec4 (f r) (f g) (f b) (f a)) <$> getUByte <*> getUByte <*> getUByte <*> getUByte
  where
    f v = fromIntegral v / 255

getInt      = fromIntegral <$> getInt' :: Get Int
  where
    getInt' = fromIntegral <$> getWord32le :: Get Int32

getInt2     = (,) <$> getInt <*> getInt

getItems elemSize a byteCount = V.replicateM (byteCount `div` elemSize) a

getHeader = do
    magic <- getString 4
    case magic == "IBSP" of
        True    -> return ()
        _       -> fail "Invalid format."
    version <- getWord
    replicateM 17 getInt2

getSurfaceType  = getInt >>= \i -> case i of
    1 -> return Planar
    2 -> return Patch
    3 -> return TriangleSoup
    4 -> return Flare
    _ -> fail "Invalid surface type"

getEntities l   = getString l
getShaders      = getItems  72 $ Shader     <$> (SB8.map toLower <$> getString 64) <*> getInt <*> getInt
getPlanes       = getItems  16 $ Plane      <$> getVec3 <*> getFloat
getNodes        = getItems  36 $ Node       <$> getInt <*> getInt2 <*> getVec3i <*> getVec3i
getLeaves       = getItems  48 $ Leaf       <$> getInt <*> getInt <*> getVec3i <*> getVec3i <*> getInt <*> getInt <*> getInt <*> getInt
getLeafSurfaces = getItems   4   getInt
getLeafBrushes  = getItems   4   getInt
getModels       = getItems  40 $ Model      <$> getVec3 <*> getVec3 <*> getInt <*> getInt <*> getInt <*> getInt
getBrushes      = getItems  12 $ Brush      <$> getInt <*> getInt <*> getInt
getBrushSides   = getItems   8 $ BrushSide  <$> getInt <*> getInt
getDrawVertices = getItems  44 $ DrawVertex <$> getVec3 <*> getVec2 <*> getVec2 <*> getVec3 <*> getVec4RGBA
getDrawIndices  = getItems   4   getInt
getFogs         = getItems  72 $ Fog        <$> getString 64 <*> getInt <*> getInt
getSurfaces     = getItems 104 $ Surface    <$> getInt <*> getInt <*> getSurfaceType <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt
                                            <*> getVec2i <*> getVec2i <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3 <*> getInt2
getLightmaps    = getItems (128*128*3) (Lightmap <$> (getByteString $ 128*128*3))

getLightGrid = getItems 8 $ do
    ambient     <- getUByte3
    directional <- getUByte3
    dir         <- getUByte2
    return LightGrid

getVisibility l = do
    nvecs   <- getInt
    szvecs  <- getInt
    vecs    <- getByteString $ nvecs * szvecs
    return $ Visibility nvecs szvecs $ V.fromList $ SB.unpack vecs

readBSP :: LB.ByteString -> BSPLevel
readBSP dat = BSPLevel
    (lump getEntities      0)
    (lump getShaders       1)
    (lump getPlanes        2)
    (lump getNodes         3)
    (lump getLeaves        4)
    (lump getLeafSurfaces  5)
    (lump getLeafBrushes   6)
    (lump getModels        7)
    (lump getBrushes       8)
    (lump getBrushSides    9)
    (lump getDrawVertices  10)
    (lump getDrawIndices   11)
    (lump getFogs          12)
    (lump getSurfaces      13)
    (lump getLightmaps     14)
    (lump getLightGrid     15)
    (lump getVisibility    16)
  where
    el = runGet getHeader dat
    lump g i = runGet (let (o,l) = el !! i in skip o >> g l) dat

loadBSP :: String -> IO BSPLevel
loadBSP n = readBSP <$> LB.readFile n
