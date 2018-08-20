{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.Dsv.Internal.BitString.Lazy where

import Control.DeepSeq                           (NFData)
import Data.Bits.BitSize
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Length
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Take
import HaskellWorks.Data.Vector.AsVector64s
import Prelude                                   hiding (drop, length, take)

import qualified Data.Vector.Storable as DVS

data BitString = BitString
  { bits      :: ![DVS.Vector Word64]
  , popCounts :: ![Count]
  } deriving (Eq, Show, Generic)

instance NFData BitString

class ToBitString a where
  toBitString :: a -> BitString

instance ToBitString BitString where
  toBitString = id
  {-# INLINE toBitString #-}

instance ToBitString [DVS.Vector Word64] where
  toBitString vs = BitString
    { bits      = vs
    , popCounts = popCount1 <$> vs
    }
  {-# INLINE toBitString #-}

instance Rank1 BitString where
  rank1 (BitString bs popCnts) = go 0 bs popCnts
    where go lpc (w:ws) (pc:pcs) p = if p <= bitCount w
            then lpc + rank1 w p
            else go (lpc + pc) ws pcs (p - bitCount w)
          go lpc _ _ _ = lpc
  {-# INLINABLE rank1 #-}

instance Select1 BitString where
  select1 (BitString vs bspcs) c = go vs bspcs c 0
    where go _ _ 0 acc = acc
          go (w:ws) (pc:pcs) d acc = if d <= pc
            then select1 w d + acc
            else go ws pcs (d - pc) (acc + bitCount w)
          go _ _ _ acc = acc
  {-# INLINE select1 #-}

instance Container BitString where
  type Elem BitString = Word64

instance Drop BitString where
  -- drop :: Count -> BitString -> BitString
  drop n (BitString (v:vs) (pc:pcs)) = if n < length v
    then BitString (drop n v:vs) (pc - popCount1 (take n v):pcs)
    else drop (n - length v) (BitString vs pcs)
  drop _ _ = BitString [] []
