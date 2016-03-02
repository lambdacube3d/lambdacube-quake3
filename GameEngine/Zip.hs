{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module GameEngine.Zip
  ( readArchive
  , readEntry
  , Entry(..)
  ) where

{-
 Zip specification:
    http://en.wikipedia.org/wiki/Zip_(file_format)
    http://www.pkware.com/documents/casestudies/APPNOTE.TXT
-}

import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.Char
import Data.Word
import Data.Int
import qualified Codec.Compression.Zlib.Raw as Zlib
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Control.Exception (evaluate)
import Control.DeepSeq
import GHC.Generics

data Entry
  = Entry
  { eArchiveName  :: !String
  , eFilePath     :: !String
  , eIsCompressed :: !Bool
  , eFilePosition :: !Int64
  , eDataSize     :: !Int64
  }
  deriving (Generic,Show)

instance NFData Entry

readEntry :: Entry -> IO SB.ByteString
readEntry Entry{..} = withBinaryFile eArchiveName ReadMode $ \handle -> do
  hSeek handle AbsoluteSeek (fromIntegral eFilePosition)
  content <- SB.hGet handle (fromIntegral eDataSize)
  return $ if eIsCompressed
    then LB.toStrict . Zlib.decompress . LB.fromStrict $ content
    else content

readArchive :: String -> IO (Map String Entry)
readArchive n = withBinaryFile n ReadMode $ \handle -> do
  ents <- runGet (getArchive n) <$> LB.hGetContents handle
  evaluate $ force $ Map.fromList [(eFilePath e, e) | e <- ents]

chunks :: Word32 -> Get a -> Get [a]
chunks c a = lookAhead getWord32le >>= \code -> case code == c of
  True  -> (:) <$> a <*> chunks c a
  False -> return $! []

getArchive :: String -> Get [Entry]
getArchive archiveName = chunks 0x04034b50 $ do
  -- local file header
  skip 6
  flag <- getWord16le
  isComp <- getWord16le >>= \i -> case i of
      0 -> return False
      8 -> return True
      _ -> fail "Unsupported compression method!"
  skip 8
  dataLength <- getWord32le
  skip 4
  nameLen <- getWord16le
  extraLen <- getWord16le
  fileName <- SB.unpack <$> getByteString (fromIntegral nameLen)
  skip $! fromIntegral extraLen
  when (flag .&. 8 /= 0) $ fail "Zip data descriptor is not supported!"
  dataPosition <- bytesRead
  skip $ fromIntegral dataLength
  return $ Entry
    { eArchiveName  = archiveName
    , eFilePath     = map toLower fileName
    , eIsCompressed = isComp
    , eFilePosition = dataPosition
    , eDataSize     = fromIntegral dataLength
    }
