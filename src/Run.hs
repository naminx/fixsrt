{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Control.Lens (imap)
import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Formatting
import Import
import Path
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import System.Directory (getCurrentDirectory)
import Text.Han2Zen
import Text.Subtitles.SRT


run :: RIO App ()
run = do
  target <- asks (.options.files)
  dirPath <- liftIO getCurrentDirectory
  case parseAbsDir dirPath of
    Left someException -> do
      logError $ display $ T.pack $ displayException someException
      return ()
    Right cwd ->
      traverse_ (fixSrt . toAbsFile cwd) target


toAbsFile :: Path Abs Dir -> SomeBase File -> Path Abs File
toAbsFile cwd someFile = case someFile of
  Abs absFile -> absFile
  Rel relFile -> cwd </> relFile


fixSrt :: Path Abs File -> RIO App ()
fixSrt file = do
  logInfo $ display $ "converting " <> T.pack (toFilePath file)
  fileContent <- liftIO $ T.readFile $ toFilePath file
  case parseOnly parseSRT fileContent of
    Left ex -> do
      logError $ display $ T.pack ex
      return ()
    Right srt -> do
      liftIO
        $ T.writeFile (toFilePath file)
        $ T.intercalate "\n"
        $ map displayLine
        $ imap
          ( \i x ->
              x
                { index = i + 1
                , dialog = T.pack $ han2zen $ T.unpack x.dialog
                }
          )
        $ compactRange
        $ compactDialog srt


compactDialog :: [Line] -> [Line]
compactDialog [] = []
compactDialog [x] = [x]
compactDialog (x : y : z) =
  if x.range /= y.range
    then x : compactDialog (y : z)
    else compactDialog (y {dialog = T.intercalate "\n" [x.dialog, y.dialog]} : z)


compactRange :: [Line] -> [Line]
compactRange [] = []
compactRange [x] = [x]
compactRange (x : y : z) =
  if x.dialog /= y.dialog || x.range.to /= y.range.from
    then x : compactRange (y : z)
    else compactRange (y {range = y.range {from = x.range.from}} : z)


displayLine :: Line -> Text
displayLine line =
  tshow (line.index)
    <> "\n"
    <> displayRange line.range
    <> "\n"
    <> line.dialog
    <> "\n"


displayRange :: Range -> Text
displayRange range_ = displayTime range_.from <> " --> " <> displayTime range_.to


displayTime :: Time -> Text
displayTime time =
  TL.toStrict $
    format
      (int02 % ":" % int02 % ":" % int02 % "," % int03)
      time.hour
      time.minutes
      time.seconds
      time.frame
 where
  int02 = lpadded 2 '0' int
  int03 = lpadded 3 '0' int
