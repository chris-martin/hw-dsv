{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.QueryLazy
  ( cmdQueryLazy
  ) where

import App.Char
import App.Commands.Options.Type
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.List
import Data.Semigroup               ((<>))
import Options.Applicative          hiding (columns)

import qualified App.IO                            as IO
import qualified App.Lens                          as L
import qualified Data.ByteString.Builder           as B
import qualified Data.Vector                       as DV
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL

runQueryLazy :: QueryLazyOptions -> IO ()
runQueryLazy opts = do
  !bs <- IO.readInputFile (opts ^. L.filePath)

  let !c = SVL.makeCursor (opts ^. L.delimiter) bs
  let !sel = opts ^. L.columns
  let !rows = SVL.selectListVector sel c
  let !outDelimiterBuilder = B.word8 (opts ^. L.outDelimiter)

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. L.outputFilePath) Nothing
    forM_ rows $ \row -> do
      let fieldStrings = fmap B.lazyByteString row

      liftIO $ B.hPutBuilder hOut $ mconcat (intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()

cmdQueryLazy :: Mod CommandFields (IO ())
cmdQueryLazy = command "query-lazy" $ flip info idm $ runQueryLazy <$> optsQueryLazy

optsQueryLazy :: Parser QueryLazyOptions
optsQueryLazy = QueryLazyOptions
    <$> many
        ( option auto
          (   long "column"
          <>  short 'k'
          <>  help "Column to select"
          <>  metavar "COLUMN INDEX" ))
    <*> strOption
          (   long "input"
          <>  short 'i'
          <>  help "Input DSV file"
          <>  metavar "FILE"
          <>  showDefault
          <>  value "-"
          )
    <*> strOption
          (   long "output"
          <>  short 'o'
          <>  help "Output DSV file"
          <>  metavar "FILE"
          <>  showDefault
          <>  value "-"
          )
    <*> option readWord8
          (   long "input-delimiter"
          <>  short 'd'
          <>  help "Input DSV delimiter"
          <>  metavar "CHAR"
          )
    <*> option readWord8
          (   long "output-delimiter"
          <>  short 'e'
          <>  help "Output DSV delimiter"
          <>  metavar "CHAR"
          )
