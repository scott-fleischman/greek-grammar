module Text.Greek.IO.Pipeline where

import System.FilePath ((</>))
import qualified Codec.Compression.GZip as GZip
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.IO.Utility as Utility
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified System.Directory as Directory

writeCompressed :: Aeson.ToJSON a => String -> a -> IO ()
writeCompressed n = ByteString.writeFile n . GZip.compress . Aeson.encode

readCompressed :: Aeson.FromJSON a => String -> Monad.ExceptT String IO a
readCompressed n = do
  bytes <- Monad.liftIO $ fmap GZip.decompress . ByteString.readFile $ n
  Utility.handleMaybe ("readCompressed " ++ n) $ Aeson.decode bytes

runSblgnt :: Monad.ExceptT String IO ()
runSblgnt = do
  Monad.liftIO $ Directory.createDirectoryIfMissing True Paths.buildSblgnt
  let write n x = Monad.liftIO $ writeCompressed (Paths.buildSblgnt </> n ++ ".json.gz") x

  sourceInfo <- All.loadSblgnt
  write "sourceInfo" sourceInfo

  let composed = toComposed sourceInfo
  write "composed" composed

toComposed
  :: [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]]
  -> [Work.Indexed [Word.Word Word.Basic [Unicode.Composed]]]
toComposed =
  Lens.over
  (traverse . Work.content . traverse . Word.surface)
  (Unicode.toComposed . Word.getSource . Word.getSourceInfoWord)
