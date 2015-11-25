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
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Script.Marked as Marked
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified System.Directory as Directory

writeCompressed :: Aeson.ToJSON a => String -> a -> IO ()
writeCompressed n = ByteString.writeFile n . GZip.compress . Aeson.encode

readCompressed :: Aeson.FromJSON a => String -> Monad.ExceptT String IO a
readCompressed n = do
  bytes <- Monad.liftIO $ fmap GZip.decompress . ByteString.readFile $ n
  Utility.handleMaybe ("readCompressed " ++ n) $ Aeson.decode bytes


data StageName
  = StageName_SourceInfo
  | StageName_Composed
  | StageName_DecomposedPairs
  | StageName_UnicodeLetterMarksPairs

stageNameToFileNamePart :: StageName -> String
stageNameToFileNamePart StageName_SourceInfo = "SourceInfo"
stageNameToFileNamePart StageName_Composed = "Composed"
stageNameToFileNamePart StageName_DecomposedPairs = "DecomposedPairs"
stageNameToFileNamePart StageName_UnicodeLetterMarksPairs = "UnicodeLetterMarksPairs"

data Stage a b e = Stage
  { stageName :: StageName
  , stageMap :: a -> Either e b
  }

runSblgnt :: Monad.ExceptT String IO ()
runSblgnt = do
  Monad.liftIO $ Directory.createDirectoryIfMissing True Paths.buildSblgnt
  let
    write s x = do
      let fileNamePart = stageNameToFileNamePart . stageName $ s
      _ <- Monad.liftIO . putStrLn $ "Writing " ++ fileNamePart
      Monad.liftIO $ writeCompressed (Paths.buildSblgnt </> fileNamePart ++ ".json.gz") x
  let
    step s x = do
      next <- Utility.handleError $ stageMap s x
      write s next
      return next

  start <- All.loadSblgnt
  sourceInfo <- step sourceInfoStage start
  composed <- step composedStage sourceInfo
  decomposed <- step decomposedPairsStage composed
  _ <- step unicodeLetterMarksPairsStage decomposed
  Monad.liftIO $ putStrLn "Complete"

sourceInfoStage :: Stage
  [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]]
  [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]]
  ()
sourceInfoStage = Stage
  { stageName = StageName_SourceInfo
  , stageMap = pure
  }

composedStage :: Stage
  [Work.Indexed [Word.Word Word.Basic Word.SourceInfo]]
  [Work.Indexed [Word.Word Word.Basic [Unicode.Composed]]]
  ()
composedStage = Stage
  { stageName = StageName_Composed
  , stageMap = pure .
    Lens.over
    (traverse . Work.content . traverse . Word.surface)
    (Unicode.toComposed . Word.getSource . Word.getSourceInfoWord)
  }

decomposedPairsStage :: Stage
  [Work.Indexed [Word.Word Word.Basic [Unicode.Composed]]]
  [Work.Indexed [Word.Word Word.Basic [(Unicode.Composed, [Unicode.Decomposed])]]]
  ()
decomposedPairsStage = Stage
  { stageName = StageName_DecomposedPairs
  , stageMap = pure .
    Lens.over
    (traverse . Work.content . traverse . Word.surface . traverse)
    (\x -> (x, Unicode.decompose' x))
  }

toDecomposedWords
  :: [Work.Indexed [Word.Word Word.Basic [(Unicode.Composed, [Unicode.Decomposed])]]]
  -> [Work.Indexed [Word.Word Word.Basic [Unicode.Decomposed]]]
toDecomposedWords =
  Lens.over
  (traverse . Work.content . traverse . Word.surface)
  (concatMap snd)

splitDecomposedElision
  :: [Work.Indexed [Word.Word Word.Basic [Unicode.Decomposed]]]
  -> [Work.Indexed [Word.Word Word.Elision [Unicode.Decomposed]]]
splitDecomposedElision = Lens.over (traverse . Work.content . traverse) go
  where
    go :: Word.Word Word.Basic [Unicode.Decomposed] -> Word.Word Word.Elision [Unicode.Decomposed]
    go w = newInfo
      where
        newInfo = Lens.over Word.info (Word.addElisionPair e) newSurface
        newSurface = Lens.set Word.surface as w
        (e, as) = Elision.split Unicode.decomposed (Word.getSurface w)

toUnicodeLetterMarksPairs
  :: [Work.Indexed [Word.Word b [Unicode.Decomposed]]]
  -> Either Unicode.Error
    [Work.Indexed [Word.Word b [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]]]
toUnicodeLetterMarksPairs = (traverse . Work.content . traverse . Word.surface) Unicode.parseMarkedLetters

unicodeLetterMarksPairsStage :: Stage
  [Work.Indexed [Word.Word Word.Basic [(Unicode.Composed, [Unicode.Decomposed])]]]
  [Work.Indexed [Word.Word Word.Elision [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]]]
  Unicode.Error
unicodeLetterMarksPairsStage = Stage
  { stageName = StageName_UnicodeLetterMarksPairs
  , stageMap = toUnicodeLetterMarksPairs . splitDecomposedElision . toDecomposedWords
  }
