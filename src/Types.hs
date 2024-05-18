{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( App (..)
  , Options (..)
  )
where

import Path
import RIO
import RIO.Process


-- | Command line arguments
data Options = Options
  { verbose :: !Bool
  , files :: ![SomeBase File]
  }


data App = App
  { logFunc :: !LogFunc
  , processContext :: !ProcessContext
  , options :: !Options
  -- Add other app-specific configuration information here
  }


instance HasLogFunc App where
  logFuncL = lens (.logFunc) $ \x y -> x {logFunc = y}


instance HasProcessContext App where
  processContextL = lens (.processContext) $ \x y -> x {processContext = y}

{-
{- |
Module      : Text.Subtitles.SRT.Datatypes
Copyright   : Ruben Astudillo 2012
License     : BSD3

Maintainer  : ruben.astud@gmail.com
Portability : unknown

ADT for .srt files. Also serves as a place to  provide instance
declarations for the ADTs.
-}

{- | This represent the position on screen of the Line. Is usually optional in
the file.
-}
data Rectangle = R {x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int}
  deriving (Eq, Ord, Show)

data Time = Time
  { hour :: Int
  , minutes :: Int
  , seconds :: Int
  , frame :: Int
  }
  deriving (Eq, Ord, Show)

data Range = Range
  { from :: Time
  , to :: Time
  }
  deriving (Eq, Ord, Show)

{- | The core of the parser. each one of the constructor representing one part
of the Line
-}
data Line = Line
  { index :: Int
  -- ^ The absolute order of this line.
  , range :: Range
  -- ^ The interval of time that the line is shown.
  , geometry :: Maybe Rectangle
  -- ^ Sometimes text shouldn't be on the lower center.
  , dialog :: Text
  -- ^ what to show in screen
  }
  deriving (Eq, Ord, Show)

-- | A subtitle is just a List of independent Lines that appear on screen
type Subtitles = [Line]

{-
instance Semigroup Line where
  x <> y =
    if x.range /= y.range
      then y
      else y {dialog = x.dialog <> "\n" <> y.dialog}
      -}
      -}
