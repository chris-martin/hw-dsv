module HaskellWorks.Data.Dsv.Lazy.Cursor
  ( DsvCursor (..)
  , makeCursor
  , makeCursor2
  , snippet
  , trim
  , atEnd
  , nextField
  , nextRow
  , nextPosition
  , getRowBetween
  , toListVector
  , toVectorVector
  ) where

import Data.Function
import Data.Word
import GHC.Word                                   (Word8)
import HaskellWorks.Data.Dsv.Internal.Bits
import HaskellWorks.Data.Dsv.Internal.Broadword   (fillWord64)
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Vector.AsVector64s
import Prelude

import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Vector                                as DV
import qualified Data.Vector.Storable                       as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Build       as B
import qualified HaskellWorks.Data.Dsv.Internal.Char.Word64 as CW

makeCursor2 :: Word8 -> LBS.ByteString -> DsvCursor
makeCursor2 delimiter lbs = DsvCursor
  { dsvCursorText      = lbs
  , dsvCursorMarkers   = ib
  , dsvCursorNewlines  = nls
  , dsvCursorPosition  = 0
  }
  where ws  = asVector64s 64 lbs
        ibq = makeIbs CW.doubleQuote         <$> ws
        ibn = makeIbs CW.newline             <$> ws
        ibd = makeIbs (fillWord64 delimiter) <$> ws
        pcq = makeCummulativePopCount ibq
        ibr = zip2Or ibn ibd
        qm  = makeQuoteMask ibq pcq
        ib  = zip2And ibr qm
        nls = zip2And ibn qm
{-# INLINE makeCursor #-}

makeCursor :: Word8 -> LBS.ByteString -> DsvCursor
makeCursor delimiter lbs = DsvCursor
  { dsvCursorText      = lbs
  , dsvCursorMarkers   = mss
  , dsvCursorNewlines  = nss
  , dsvCursorPosition  = 0
  }
  where ws  = asVector64s 64 lbs
        buildIbs :: [DVS.Vector Word64] -> Word64 -> [(DVS.Vector Word64, DVS.Vector Word64)]
        buildIbs (v:vs) qc = let (ms, ns, nqc) = B.buildIbs delimiter qc v in (ms, ns):buildIbs vs nqc
        buildIbs [] _      = []
        (mss, nss) = unzip (buildIbs ws 0)
{-# INLINE makeCursor2 #-}

snippet :: DsvCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ dsvCursorText c
  where d = nextField c
        posC = fromIntegral $ dsvCursorPosition c
        posD = fromIntegral $ dsvCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

trim :: DsvCursor -> DsvCursor
trim c = if dsvCursorPosition c >= 512
  then trim c
    { dsvCursorText     = LBS.drop 512 (dsvCursorText c)
    , dsvCursorMarkers  = drop 1 (dsvCursorMarkers c)
    , dsvCursorNewlines = drop 1 (dsvCursorNewlines c)
    , dsvCursorPosition = dsvCursorPosition c - 512
    }
  else c
{-# INLINE trim #-}

atEnd :: DsvCursor -> Bool
atEnd c = LBS.null (LBS.drop (fromIntegral (dsvCursorPosition c)) (dsvCursorText c))
{-# INLINE atEnd #-}

nextField :: DsvCursor -> DsvCursor
nextField cursor = cursor
  { dsvCursorPosition = newPos
  }
  where currentRank = rank1   (dsvCursorMarkers cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorMarkers cursor) (currentRank + 1) - 1
{-# INLINE nextField #-}

nextRow :: DsvCursor -> DsvCursor
nextRow cursor = cursor
  { dsvCursorPosition = if newPos > dsvCursorPosition cursor
                          then newPos
                          else fromIntegral (LBS.length (dsvCursorText cursor))

  }
  where currentRank = rank1   (dsvCursorNewlines cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorNewlines cursor) (currentRank + 1) - 1
{-# INLINE nextRow #-}

nextPosition :: DsvCursor -> DsvCursor
nextPosition cursor = cursor
    { dsvCursorPosition = if LBS.null (LBS.drop (fromIntegral newPos) (dsvCursorText cursor))
                            then fromIntegral (LBS.length (dsvCursorText cursor))
                            else newPos
    }
  where newPos  = dsvCursorPosition cursor + 1
{-# INLINE nextPosition #-}

getRowBetween :: DsvCursor -> DsvCursor -> Bool -> DV.Vector LBS.ByteString
getRowBetween c d dEnd = DV.unfoldrN fields go c
  where cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
        go :: DsvCursor -> Maybe (LBS.ByteString, DsvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowBetween #-}

toListVector :: DsvCursor -> [DV.Vector LBS.ByteString]
toListVector c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowBetween c d dEnd:toListVector (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toListVector #-}

toVectorVector :: DsvCursor -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
