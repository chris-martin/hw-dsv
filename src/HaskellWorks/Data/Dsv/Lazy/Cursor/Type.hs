{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.Dsv.Lazy.Cursor.Type where

import Control.DeepSeq                               (NFData)
import Data.Word
import GHC.Generics                                  (Generic)
import HaskellWorks.Data.Dsv.Internal.BitString.Lazy

import qualified Data.ByteString.Lazy as LBS

data DsvCursor = DsvCursor
  { dsvCursorText     :: !LBS.ByteString
  , dsvCursorMarkers  :: !BitString
  , dsvCursorNewlines :: !BitString
  , dsvCursorPosition :: !Word64
  } deriving (Eq, Show, Generic)

instance NFData DsvCursor
