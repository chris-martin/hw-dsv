module HaskellWorks.Data.Dsv.Internal.LazyCursor where

import HaskellWorks.Data.Dsv.Lazy.Cursor
import HaskellWorks.Data.RankSelect.Base.Rank1
import Prelude

getRowCount :: DsvCursor -> DsvCursor -> Bool -> Int
getRowCount c d dEnd = fields
  where cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
{-# INLINE getRowCount #-}

toRowCounts :: DsvCursor -> [Int]
toRowCounts c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowCount c d dEnd:toRowCounts (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toRowCounts #-}
