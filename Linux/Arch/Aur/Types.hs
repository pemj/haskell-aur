module Linux.Arch.Aur.Types where

import Data.Text

---

data AurInfo = AurInfo { aurIdOf          :: Int
                       , aurNameOf        :: Text
                         -- This should be `Version` and parsed nicely.
                       , aurVersionOf     :: Text
                       , aurCategoryOf    :: Int
                       , aurDescriptionOf :: Text
                       , urlOf            :: Text
                       , aurVotesOf       :: Int
                       , isOutOfDate      :: Bool
                       , aurMaintainerOf  :: Text
                       , submissionDatOf  :: Text  -- Text?
                       , modifiedDateOf   :: Text
                       , aurTarballUrlOf  :: Text
                       , dependsOf        :: [Text]
                       , makeDepsOf       :: [Text]
                       , optDepsOf        :: [Text]
                       , conflictsOf      :: [Text]
                       , providesOf       :: [Text] } deriving (Eq,Show)
