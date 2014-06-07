module Linux.Arch.Aur.Types
    ( AurInfo(..) ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (maybeToList)
import Data.Text

import qualified Data.List as L

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
    parseJSON (Object v) = AurInfo                     <$>
                           v .: "ID"                   <*>
                           v .: "Name"                 <*>
                           v .: "PackageBaseID"        <*>
                           v .: "PackageBase"          <*>
                           v .: "Version"              <*>
                           v .: "CategoryID"           <*>
                           v .: "Description"          <*>
                           v .: "License"              <*>
                           v .: "URL"                  <*>
                           v .: "NumVotes"             <*>
                           v .: "OutOfDate"            <*>
                           v .: "Maintainer"           <*>
                           v .: "FirstSubmitted"       <*>
                           v .: "LastModified"         <*>
                           v .: "URLPath"              <*>
                           (f <$> v .:? "Depends")     <*>
                           (f <$> v .:? "MakeDepends") <*>
                           (f <$> v .:? "OptDepends")  <*>
                           (f <$> v .:? "Conflicts")   <*>
                           (f <$> v .:? "Provides")
                               where f = L.concat . maybeToList
    parseJSON _          = mzero
