module App.Commands.IndexWord8s
  ( cmdIndexWord8s
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.ByteString.Lazy
import HaskellWorks.Data.Vector.AsVector64
import HaskellWorks.Data.Vector.AsVector64s
import HaskellWorks.Data.Vector.AsVector8ns
import Options.Applicative

import qualified App.IO                                    as IO
import qualified App.Lens                                  as L
import qualified Data.ByteString.Lazy                      as LBS
import qualified HaskellWorks.Data.Dsv.Internal.ByteString as BS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2    as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock   as STOCK

runIndexWord8sNormal :: IndexWord8sOptions -> IO ()
runIndexWord8sNormal opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & toLazyByteString
    . fmap (STOCK.cmpEqWord8s 44 . asVector64)
    . BS.rechunkPaddedAlignedAt 64
    . LBS.toChunks

runIndexWord8sSimd :: IndexWord8sOptions -> IO ()
runIndexWord8sSimd opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & toLazyByteString
    . fmap (AVX2.cmpEqWord8s 44 . asVector64)
    . BS.rechunkPaddedAlignedAt 64
    . LBS.toChunks

runIndexWord8sNormal2 :: IndexWord8sOptions -> IO ()
runIndexWord8sNormal2 opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & toLazyByteString
    . fmap (STOCK.cmpEqWord8s 44)
    . asVector64s 64
    . LBS.toChunks

runIndexWord8sSimd2 :: IndexWord8sOptions -> IO ()
runIndexWord8sSimd2 opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & toLazyByteString
    . fmap (AVX2.cmpEqWord8s 44)
    . asVector64s 64
    . LBS.toChunks

runIndexWord8sNormal3 :: IndexWord8sOptions -> IO ()
runIndexWord8sNormal3 opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & toLazyByteString
    . fmap (STOCK.cmpEqWord8s 44)
    . asVector8ns 512
    . LBS.toChunks

runIndexWord8sSimd3 :: IndexWord8sOptions -> IO ()
runIndexWord8sSimd3 opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & toLazyByteString
    . fmap (AVX2.cmpEqWord8s 44)
    . asVector8ns 512
    . LBS.toChunks

runIndexWord8s :: IndexWord8sOptions -> IO ()
runIndexWord8s opts = case opts ^. L.method of
  "simd"   -> runIndexWord8sSimd    opts
  "stock"  -> runIndexWord8sNormal  opts
  "simd2"  -> runIndexWord8sSimd2   opts
  "stock2" -> runIndexWord8sNormal2 opts
  "simd3"  -> runIndexWord8sSimd3   opts
  "stock3" -> runIndexWord8sNormal3 opts
  method   -> error $ "Unrecognised method: " <> method

optsIndexWord8s :: Parser IndexWord8sOptions
optsIndexWord8s = IndexWord8sOptions
  <$> strOption
        (   long "input"
        <>  help "Input file"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "-"
        )
  <*> strOption
        (   long "output"
        <>  help "Output file"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "-"
        )
  <*> strOption
        (   long "method"
        <>  help "Method"
        <>  metavar "METHOD"
        <>  showDefault
        <>  value "stock"
        )

cmdIndexWord8s :: Mod CommandFields (IO ())
cmdIndexWord8s = command "index-word8s"  $ flip info idm $ runIndexWord8s <$> optsIndexWord8s
