{-# LANGUAGE OverloadedStrings #-}
module GameEngine.Loader.MD3
  ( readMD3
  , loadMD3
  ) where

import Control.Monad
import Data.Int

import qualified Data.HashMap.Strict as HashMap
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import GameEngine.Data.MD3

getString :: Int -> Get SB.ByteString
getString = fmap (SB.takeWhile (/= '\0')) . getByteString

getUByte    = B.get :: Get Word8
getFloat    = getFloat32le :: Get Float
getVec2     = Vec2 <$> getFloat <*> getFloat :: Get Vec2
getVec3     = Vec3 <$> getFloat <*> getFloat <*> getFloat :: Get Vec3
getVec3i16  = (\x y z -> Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)) <$> getInt16 <*> getInt16 <*> getInt16 :: Get Vec3
getInt16    = fromIntegral <$> getInt' :: Get Int
  where getInt' = fromIntegral <$> getWord16le :: Get Int16
getInt      = fromIntegral <$> getInt' :: Get Int
  where getInt' = fromIntegral <$> getWord32le :: Get Int32
getInt3     = (,,) <$> getInt <*> getInt <*> getInt :: Get (Int, Int, Int)
getAngle    = (\i -> fromIntegral i * 2 * pi / 255) <$> getUByte :: Get Float

getV :: Int -> Int -> Get a -> LB.ByteString -> V.Vector a
getV o n f dat = runGet (V.replicateM n f) (LB.drop (fromIntegral o) dat)

getSV :: SV.Storable a => Int -> Int -> Get a -> LB.ByteString -> SV.Vector a
getSV o n f dat = runGet (SV.replicateM n f) (LB.drop (fromIntegral o) dat)

getFrame    = Frame <$> getVec3 <*> getVec3 <*> getVec3 <*> getFloat <*> getString 64 :: Get Frame
getTag      = Tag <$> getString 64 <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3 :: Get Tag
getShader   = Shader <$> getString 64 <*> getInt  :: Get Shader

getXyzNormal :: Get (Vec3, Vec3)
getXyzNormal = do
    v <- getVec3i16
    lat <- getAngle
    lng <- getAngle
    return (v &* (1/64), Vec3 (cos lat * sin lng) (sin lat * sin lng) (cos lng))

getSurface :: Get Surface
getSurface = (\(o,v) -> skip o >> return v) =<< lookAhead getSurface'
  where
    getSurface' = do
        dat <- lookAhead getRemainingLazyByteString
        "IDP3" <- getString 4
        name <- getString 64
        flags <- getInt
        [nFrames,nShaders,nVerts,nTris] <- replicateM 4 getInt
        [oTris,oShaders,oTexCoords,oXyzNormals,oEnd] <- replicateM 5 getInt
        return $ (oEnd,Surface name
          (getV oShaders nShaders getShader dat)
          (getSV oTris (3*nTris) (fromIntegral <$> getWord32le) dat)
          (getSV oTexCoords nVerts getVec2 dat)
          (getV oXyzNormals nFrames ((\(p,n) -> (SV.fromList p,SV.fromList n)) . unzip <$> replicateM nVerts getXyzNormal) dat))

getMD3Model :: Get MD3Model
getMD3Model = do
    dat <- lookAhead getRemainingLazyByteString
    "IDP3" <- getString 4
    version <- getInt
    when (version /= 15) $ fail "unsupported md3 version"
    name <- getString 64
    flags <- getInt
    [nFrames,nTags,nSurfaces,nSkins] <- replicateM 4 getInt
    [oFrames,oTags,oSurfaces,oEnd] <- replicateM 4 getInt
    return $ MD3Model
      { mdFrames    = getV oFrames nFrames getFrame dat
      , mdTags      = (\v -> HashMap.fromList [(tgName t,t) | t <- V.toList v]) <$> getV oTags nFrames (V.replicateM nTags getTag) dat
      , mdSurfaces  = getV oSurfaces nSurfaces getSurface dat
      }

loadMD3 :: String -> IO MD3Model
loadMD3 n = readMD3 <$> LB.readFile n

readMD3 :: LB.ByteString -> MD3Model
readMD3 dat = runGet getMD3Model dat
