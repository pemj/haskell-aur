module Linux.Arch.Aur.Types
    ( AurInfo(..) ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Text

---

data AurInfo = AurInfo { aurIdOf          :: Int
                       , aurNameOf        :: Text
                       , pkgBaseIdOf      :: Int
                       , pkgBaseOf        :: Text
                       , aurVersionOf     :: Text
                       , aurCategoryOf    :: Int
                       , aurDescriptionOf :: Text
                       , licenseOf        :: [Text]
                       , urlOf            :: Text
                       , aurVotesOf       :: Int
                       , isOutOfDate      :: Int
                       , aurMaintainerOf  :: Text
                       , submissionDatOf  :: Int
                       , modifiedDateOf   :: Int
                       , aurTarballUrlOf  :: Text
                       , dependsOf        :: [Text]
                       , makeDepsOf       :: [Text]
                       , optDepsOf        :: [Text]
                       , conflictsOf      :: [Text]
                       , providesOf       :: [Text] } deriving (Eq,Show)

instance FromJSON AurInfo where
    parseJSON (Object v) = AurInfo                    <$>
                           v .:  "ID"                 <*>
                           v .:  "Name"               <*>
                           v .:  "PackageBaseID"      <*>
                           v .:  "PackageBase"        <*>
                           v .:  "Version"            <*>
                           v .:  "CategoryID"         <*>
                           v .:  "Description"        <*>
                           v .:  "License"            <*>
                           v .:  "URL"                <*>
                           v .:  "NumVotes"           <*>
                           v .:  "OutOfDate"          <*>
                           v .:  "Maintainer"         <*>
                           v .:  "FirstSubmitted"     <*>
                           v .:  "LastModified"       <*>
                           v .:  "URLPath"            <*>
                           v .:? "Depends"     .!= [] <*>
                           v .:? "MakeDepends" .!= [] <*>
                           v .:? "OptDepends"  .!= [] <*>
                           v .:? "Conflicts"   .!= [] <*>
                           v .:? "Provides"    .!= []
    parseJSON _          = mzero
