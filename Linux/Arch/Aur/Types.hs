module Linux.Arch.Aur.Types where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
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

instance FromJSON AurInfo where
    parseJSON (Object v) = undefined  --AurInfo <$>
    parseJSON _          = mzero
