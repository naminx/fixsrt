{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import Path
import qualified Paths_fixsrt
import RIO.Process
import Run


main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_fixsrt.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> some
            ( argument
                (eitherReader $ first displayException . parseSomeFile)
                (metavar "FILE.srt [MORE.srt ...]")
            )
      )
      empty
  lo <- logOptionsHandle stderr options.verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { logFunc = lf
            , processContext = pc
            , options = options
            }
     in runRIO app run
