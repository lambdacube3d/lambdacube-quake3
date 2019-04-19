-- from stb-image

{-# LANGUAGE ForeignFunctionInterface #-}
module GameEngine.Loader.Image
  ( decodeImage
  , decodeImage'
  , loadImage
  , loadImage'
  ) where

import Codec.Picture.Types
import qualified Data.Vector.Storable as SV

import Control.Monad (liftM)
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign
import Foreign.C
import System.IO
import System.IO.Error

import Data.ByteString.Internal

--------------------------------------------------------------------------------

foreign import ccall safe "stb_image.h stbi_load_from_memory"
  stbi_load_from_memory :: Ptr Word8 -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> IO (Ptr Word8)

foreign import ccall safe "stb_image.h &stbi_image_free"
  stbi_image_free :: FunPtr (Ptr a -> IO ())

foreign import ccall safe "stb_image.h stbi_failure_reason"
  stbi_failure_reason :: IO (Ptr CChar)

--------------------------------------------------------------------------------

-- If the operation fails, we return an error message.
decodeImage :: ByteString -> IO (Either String DynamicImage)
decodeImage = decodeImage' 0

-- | Decodes an image, with the number of components per pixel forced by the user.
decodeImage' :: Int -> ByteString -> IO (Either String DynamicImage)
decodeImage' forcecomp bs = do
  let (fptr,ofs,len) = toForeignPtr bs
  withForeignPtr fptr $ \q -> do
    let ptr = plusPtr q ofs
    alloca $ \pxres -> alloca $ \pyres -> alloca $ \pcomp -> do
      r <- stbi_load_from_memory ptr (fromIntegral len) pxres pyres pcomp (fromIntegral forcecomp)
      if r == nullPtr
        then do
          e <- stbi_failure_reason
          msg <- peekCString e
          return $ Left msg
        else do
          fr <- newForeignPtr stbi_image_free r 
          xres <- liftM fromIntegral $ peek pxres
          yres <- liftM fromIntegral $ peek pyres
          comp <- liftM fromIntegral $ peek pcomp
          let pixelVector = SV.unsafeFromForeignPtr0 fr (xres*yres*comp)
          return $ case comp of
            1 -> Right . ImageY8 $ Image xres yres pixelVector
            2 -> Right . ImageYA8 $ Image xres yres pixelVector
            3 -> Right . ImageRGB8 $ Image xres yres pixelVector
            4 -> Right . ImageRGBA8 $ Image xres yres pixelVector
            _ -> Left $ "unsupported image component count: " ++ show comp

ioHandler :: IOException -> IO (Either String a)
ioHandler ioerror = return $ Left $ "IO error: " ++ ioeGetErrorString ioerror

-- | Loads an image from a file. Catches IO exceptions and converts them to an error message.  
loadImage :: FilePath -> IO (Either String DynamicImage)
loadImage path = handle ioHandler $ do
  h <- openBinaryFile path ReadMode 
  b <- B.hGetContents h
  hClose h
  decodeImage b

-- | Force the number of components in the image.
loadImage':: FilePath -> Int -> IO (Either String DynamicImage)
loadImage' path ncomps = handle ioHandler $ do
  h <- openBinaryFile path ReadMode 
  b <- B.hGetContents h
  hClose h
  decodeImage' ncomps b
