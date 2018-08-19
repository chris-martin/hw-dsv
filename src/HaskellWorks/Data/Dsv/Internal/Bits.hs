module HaskellWorks.Data.Dsv.Internal.Bits where

import Data.Bits.Pext
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable as DVS

testWord8s :: Word64 -> Word64
testWord8s w =  let w8s = w
                    w4s = w8s .|. (w8s .>. 4)
                    w2s = w4s .|. (w4s .>. 2)
                    w1s = w2s .|. (w2s .>. 1)
                in  pext w1s 0x0101010101010101
{-# INLINE testWord8s #-}

zipOr :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
zipOr as bs = if DVS.length as == DVS.length bs
  then DVS.constructN len go
  else error "Different sized vectors"
  where len = DVS.length as `min` DVS.length bs
        go :: DVS.Vector Word64 -> Word64
        go u = let ui = DVS.length u in DVS.unsafeIndex as ui .|. DVS.unsafeIndex bs ui
{-# INLINE zipOr #-}

zip2Or :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
zip2Or (a:as) (b:bs) = zipOr a b:zip2Or as bs
zip2Or _     _       = []
{-# INLINE zip2Or #-}

zipAnd :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
zipAnd as bs = if DVS.length as == DVS.length bs
  then DVS.constructN len go
  else error "Different sized vectors"
  where len = DVS.length as `min` DVS.length bs
        go :: DVS.Vector Word64 -> Word64
        go u = let ui = DVS.length u in DVS.unsafeIndex as ui .&. DVS.unsafeIndex bs ui
{-# INLINE zipAnd #-}

zip2And :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
zip2And (a:as) (b:bs) = zipAnd a b:zip2And as bs
zip2And _      _      = []
{-# INLINE zip2And #-}
