{-# LANGUAGE ForeignFunctionInterface #-}

module Cbits.Interface.Driver where

import Foreign
import Foreign.C.String (withCString, CString)
import Foreign.C.Types
import Foreign.Marshal.Array (newArray)

data Sample = Sample {
	sample :: Ptr CFloat,
	sample_count :: CSize,
	current_position :: CSize,
	completed :: CBool
} deriving Show

instance Storable Sample where
    sizeOf _ = ptrSize + 2 * sizeOf (undefined :: CSize) + sizeOf (undefined :: CBool)
      where
        ptrSize = sizeOf (undefined :: Ptr CFloat)

    alignment _ = alignment (undefined :: Ptr CFloat)

    peek ptr = do
        s <- peekByteOff ptr 0
        cnt <- peekByteOff ptr offset1
        pos <- peekByteOff ptr offset2
        comp <- peekByteOff ptr offset3
        return (Sample s cnt pos comp)
      where
        offset1 = sizeOf (undefined :: Ptr CFloat)
        offset2 = offset1 + sizeOf (undefined :: CSize)
        offset3 = offset2 + sizeOf (undefined :: CSize)

    poke ptr (Sample s cnt pos comp) = do
        pokeByteOff ptr 0 s
        pokeByteOff ptr offset1 cnt
        pokeByteOff ptr offset2 pos
        pokeByteOff ptr offset3 comp
      where
        offset1 = sizeOf (undefined :: Ptr CFloat)
        offset2 = offset1 + sizeOf (undefined :: CSize)
        offset3 = offset2 + sizeOf (undefined :: CSize)

foreign import ccall "init_audio_driver" c_init_audio_driver :: CString -> IO (CInt)
foreign import ccall unsafe "write_audio" c_write_audio :: Ptr Sample -> IO ()
foreign import ccall unsafe "testI" c_testI :: IO (CInt)

initDriver :: String -> IO (Int)
initDriver driverName =
  withCString driverName $ \cstr ->
    fmap fromIntegral (c_init_audio_driver cstr)

testI :: IO (Int)
testI = fmap fromIntegral c_testI

createSample :: IO Sample
createSample = do
  let freqs :: [CFloat]
      freqs = [440.0, 880.0, 1760.0, 3520.0]
  ptr <- newArray freqs
  let count = fromIntegral (length freqs)
  return Sample
    { sample = ptr
    , sample_count = count
    , current_position = 0
    , completed = 0
    }

writeAudio :: IO (Sample) -> IO ()
writeAudio sam = sam >>= \s -> with s c_write_audio
