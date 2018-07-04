module HaskellWorks.Data.Dsv.Internal.Foreign
  ( buildIbs
  ) where

import Foreign
import Foreign.C.Types

#include "../cbits/simd.h"

buildIbs :: CChar -> CULLong -> Ptr CChar -> CULong -> Ptr CULLong -> Ptr CULLong -> IO CULLong
buildIbs = {#call unsafe build_ibs as c_build_ibs#}
