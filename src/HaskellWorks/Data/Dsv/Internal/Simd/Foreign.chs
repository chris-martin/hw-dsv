{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Foreign where

import Foreign
import Foreign.C.Types
import HaskellWorks.Data.Dsv.Internal.Simd.Capabilities

#include "../cbits/simd.h"

avx2Memcpy :: Ptr CUChar -> Ptr CUChar -> CULong -> IO ()
avx2Memcpy target source len = requireAvx2 $ do
  {#call unsafe avx2_memcpy as c_build_ibs#} target source len
{-# INLINE avx2Memcpy #-}

avx2Cmpeq8 :: CUChar -> Ptr CULong -> CULong -> Ptr CUChar -> IO ()
avx2Cmpeq8 byte target targetLength source = requireAvx2 $ do
  {#call unsafe avx2_cmpeq8 as c_cmpeq8#} byte target targetLength source
