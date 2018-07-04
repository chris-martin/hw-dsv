module HaskellWorks.Data.Dsv.Internal.Build
  ( buildIbs
  ) where

import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Unsafe

import qualified Data.ByteString                        as BS
import qualified Data.Vector.Storable                   as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Foreign as F

buildIbs :: Word8 -> Word64 -> ByteString -> (DVS.Vector Word64, DVS.Vector Word64, Word64)
buildIbs c quoteCount bs = unsafeLocalState $ do
  unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) -> do
    let bsW64Len = bsLen `div` 64
    markersForeignPtr   <- mallocForeignPtrBytes (BS.length bs `div` 64)
    newlinesForeignPtr  <- mallocForeignPtrBytes (BS.length bs `div` 64)
    withForeignPtr markersForeignPtr $ \markersPtr -> do
      withForeignPtr newlinesForeignPtr $ \newlinesPtr -> do
        CULLong newQuoteCount <- F.buildIbs (CUChar c) (CULLong quoteCount) bsPtr (CULong (fromIntegral bsLen)) markersPtr newlinesPtr
        return
          ( DVS.unsafeCast (DVS.unsafeFromForeignPtr markersForeignPtr  0 bsW64Len)
          , DVS.unsafeCast (DVS.unsafeFromForeignPtr newlinesForeignPtr 0 bsW64Len)
          , newQuoteCount)
{-# INLINE buildIbs #-}
