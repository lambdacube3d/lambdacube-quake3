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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad

import GameEngine.Data.BSP

{-
Information:
  http://graphics.stanford.edu/~kekoa/q3/
  http://www.mralligator.com/q3/
-}

getLowerCaseString :: Int -> Get ByteString
getLowerCaseString len = SB8.map toLower . SB8.takeWhile (/= '\0') <$> getByteString len

getFloat  = getFloat32le :: Get Float
getWord   = getWord32le :: Get Word32
getUByte  = B.get :: Get Word8
getUByte2 = B.get :: Get (Word8,Word8)
getUByte3 = B.get :: Get (Word8,Word8,Word8)

getVec2   = Vec2 <$> getFloat <*> getFloat :: Get Vec2
getVec3   = Vec3 <$> getFloat <*> getFloat <*> getFloat :: Get Vec3
getVec2i  = (\x y -> Vec2 (fromIntegral x) (fromIntegral y)) <$> getInt <*> getInt :: Get Vec2
getVec3i  = (\x y z -> Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)) <$> getInt <*> getInt <*> getInt :: Get Vec3

getVec4RGBA = (\r g b a -> Vec4 (f r) (f g) (f b) (f a)) <$> getUByte <*> getUByte <*> getUByte <*> getUByte :: Get Vec4
  where f v = fromIntegral v / 255

getInt = fromIntegral <$> getInt' :: Get Int
  where getInt' = fromIntegral <$> getWord32le :: Get Int32

getInt2 = (,) <$> getInt <*> getInt :: Get (Int,Int)

getItems :: Monad m => Int -> m a -> Int -> m (Vector a)
getItems elemSize a byteCount = V.replicateM (byteCount `div` elemSize) a

getHeader :: Get [(Int, Int)]
getHeader = do
    magic <- getByteString 4
    case magic == "IBSP" of
        True    -> return ()
        _       -> fail "Invalid format."
    version <- getWord
    replicateM 17 getInt2

getSurfaceType :: Get SurfaceType
getSurfaceType  = getInt >>= \i -> case i of
    1 -> return Planar
    2 -> return Patch
    3 -> return TriangleSoup
    4 -> return Flare
    _ -> fail "Invalid surface type"

getEntities l   = getLowerCaseString l :: Get ByteString
getShaders      = getItems  72 $ Shader     <$> getLowerCaseString 64 <*> getInt <*> getInt                     :: Int -> Get (Vector Shader)
getPlanes       = getItems  16 $ Plane      <$> getVec3 <*> getFloat                                            :: Int -> Get (Vector Plane)
getNodes        = getItems  36 $ Node       <$> getInt <*> getInt2 <*> getVec3i <*> getVec3i                    :: Int -> Get (Vector Node)
getLeaves       = getItems  48 $ Leaf       <$> getInt <*> getInt <*> getVec3i <*> getVec3i <*> getInt
                                            <*> getInt <*> getInt <*> getInt                                    :: Int -> Get (Vector Leaf)
getLeafSurfaces = getItems   4   getInt                                                                         :: Int -> Get (Vector Int)
getLeafBrushes  = getItems   4   getInt
getModels       = getItems  40 $ Model      <$> getVec3 <*> getVec3 <*> getInt <*> getInt <*> getInt <*> getInt :: Int -> Get (Vector Model)
getBrushes      = getItems  12 $ Brush      <$> getInt <*> getInt <*> getInt                                    :: Int -> Get (Vector Brush)
getBrushSides   = getItems   8 $ BrushSide  <$> getInt <*> getInt                                               :: Int -> Get (Vector BrushSide)
getDrawVertices = getItems  44 $ DrawVertex <$> getVec3 <*> getVec2 <*> getVec2 <*> getVec3 <*> getVec4RGBA     :: Int -> Get (Vector DrawVertex)
getDrawIndices  = getItems   4   getInt                                                                         :: Int -> Get (Vector Int)
getFogs         = getItems  72 $ Fog        <$> getLowerCaseString 64 <*> getInt <*> getInt
getSurfaces     = getItems 104 $ Surface    <$> getInt <*> getInt <*> getSurfaceType <*> getInt <*> getInt
                                            <*> getInt <*> getInt <*> getInt <*> getVec2i <*> getVec2i
                                            <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3 <*> getInt2         :: Int -> Get (Vector Surface)
getLightmaps    = getItems (128*128*3) (Lightmap <$> (getByteString $ 128*128*3))                               :: Int -> Get (Vector Lightmap)

getLightGrid :: Int -> Get (Vector LightGrid)
getLightGrid = getItems 8 $ do
    ambient     <- getUByte3
    directional <- getUByte3
    dir         <- getUByte2
    return LightGrid

getVisibility :: Int -> Get Visibility
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
    el = runGet getHeader dat :: [(Int, Int)]
    lump g i = runGet (let (o,l) = el !! i in skip o >> g l) dat

loadBSP :: String -> IO BSPLevel
loadBSP n = readBSP <$> LB.readFile n
