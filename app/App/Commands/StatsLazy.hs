{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.StatsLazy
  ( cmdStatsLazy
  ) where

import App.Char
import App.Commands.Options.Type
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Semigroup               ((<>))
import Options.Applicative          hiding (columns)

import qualified App.IO                                    as IO
import qualified App.Lens                                  as L
import qualified HaskellWorks.Data.Dsv.Internal.LazyCursor as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor         as SVL
import qualified System.IO                                 as IO

runStatsLazy :: StatsLazyOptions -> IO ()
runStatsLazy opts = do
  !bs <- IO.readInputFile (opts ^. L.filePath)

  let !c = SVL.makeCursor (opts ^. L.delimiter) bs
  let !rowCounts = SVL.toRowCounts c

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. L.outputFilePath) Nothing
    forM_ rowCounts $ \rowCount -> do
      liftIO $ IO.hPutStrLn hOut $ show rowCount

      return ()
  return ()

cmdStatsLazy :: Mod CommandFields (IO ())
cmdStatsLazy = command "stats-lazy" $ flip info idm $ runStatsLazy <$> optsStatsLazy

optsStatsLazy :: Parser StatsLazyOptions
optsStatsLazy = StatsLazyOptions
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
