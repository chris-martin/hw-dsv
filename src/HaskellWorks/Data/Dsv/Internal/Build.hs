module HaskellWorks.Data.Dsv.Internal.Build
  ( buildIbs
  ) where

import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Unsafe

import qualified Data.Vector.Storable                   as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Foreign as F

buildIbs :: Word8 -> Word64 -> DVS.Vector Word64 -> (DVS.Vector Word64, DVS.Vector Word64, Word64)
buildIbs c quoteCount bs = unsafeLocalState $ do
  let v = DVS.unsafeCast bs :: DVS.Vector CChar
  let vLen = DVS.length v
  let bsW64Len = vLen `div` 64
  DVS.unsafeWith v $ \vPtr -> do
    markersForeignPtr   <- mallocForeignPtrBytes bsW64Len
    newlinesForeignPtr  <- mallocForeignPtrBytes bsW64Len
    withForeignPtr markersForeignPtr $ \markersPtr -> do
      withForeignPtr newlinesForeignPtr $ \newlinesPtr -> do
        CULLong newQuoteCount <- F.buildIbs (CUChar c) (CULLong quoteCount) vPtr (CULong (fromIntegral vLen)) markersPtr newlinesPtr
        return
          ( DVS.unsafeCast (DVS.unsafeFromForeignPtr markersForeignPtr  0 bsW64Len)
          , DVS.unsafeCast (DVS.unsafeFromForeignPtr newlinesForeignPtr 0 bsW64Len)
          , newQuoteCount)
{-# INLINE buildIbs #-}
