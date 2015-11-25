module Text.Greek.IO.Pipeline where

import System.FilePath ((</>))
import qualified Codec.Compression.GZip as GZip
import qualified Control.Monad.Except as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.IO.Utility as Utility
import qualified Text.Greek.Source.All as All
import qualified System.Directory as Directory

writeCompressed :: Aeson.ToJSON a => String -> a -> IO ()
writeCompressed n = ByteString.writeFile (Paths.buildData </> n) . GZip.compress . Aeson.encode

readCompressed :: Aeson.FromJSON a => String -> Monad.ExceptT String IO a
readCompressed n = do
  bytes <- Monad.liftIO $ fmap GZip.decompress . ByteString.readFile $ Paths.buildData </> n
  Utility.handleMaybe ("readCompressed " ++ n) $ Aeson.decode bytes

runSblgnt :: Monad.ExceptT String IO ()
runSblgnt = do
  sourceWords <- All.loadSblgnt
  Monad.liftIO $ Directory.createDirectoryIfMissing True Paths.buildData
  Monad.liftIO . putStrLn $ "sourceWords " ++ (show . length $ sourceWords)
