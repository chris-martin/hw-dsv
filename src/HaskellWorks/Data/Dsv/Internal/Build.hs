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
buildIbs c quoteCount w64s =
  let w64sLen = DVS.length w64s in
  if w64sLen `mod` 8 == 0
    then unsafeLocalState $ do
      let w8s = DVS.unsafeCast w64s :: DVS.Vector CChar
      let w8sLen = DVS.length w8s
      DVS.unsafeWith w8s $ \w8sPtr -> do
        markersForeignPtr   <- mallocForeignPtrBytes w64sLen
        newlinesForeignPtr  <- mallocForeignPtrBytes w64sLen
        withForeignPtr markersForeignPtr $ \markersPtr -> do
          withForeignPtr newlinesForeignPtr $ \newlinesPtr -> do
            CULLong newQuoteCount <- F.buildIbs (CUChar c) (CULLong quoteCount) w8sPtr (CULong (fromIntegral w8sLen)) markersPtr newlinesPtr
            return
              ( DVS.unsafeCast (DVS.unsafeFromForeignPtr markersForeignPtr  0 w64sLen)
              , DVS.unsafeCast (DVS.unsafeFromForeignPtr newlinesForeignPtr 0 w64sLen)
              , newQuoteCount)
    else if w64sLen < 8
      then let v2 = DVS.concat [w64s, DVS.replicate 8 0] in buildIbs c quoteCount v2
      else error "Not implemented"
{-# INLINE buildIbs #-}
