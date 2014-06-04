{-# LANGUAGE OverloadedStrings #-}

module Linux.Arch.Aur
       ( -- * QUERIES
         search
       , info
       , multiinfo
       , msearch ) where

import Linux.Arch.Aur.Types

import Control.Applicative  ((<$>))
import Control.Lens
import Data.Aeson           (Value(..))
import Data.Aeson.Lens      (AsValue, _String, key, nth)
import Data.Aeson.Encode.Pretty
--import Data.ByteString.Lazy (ByteString)
import Data.Map.Lazy
import Data.Maybe           (listToMaybe)
import Data.Text
import Data.Text.Encoding
import Network.Wreq

--import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V

---

rpcUrl :: String -> String
rpcUrl method = "https://aur.archlinux.org/rpc.php?"
                ++ apiVersion
                ++ "&type=" ++ method ++ "&"

apiVersion = "v=2"

-- | Yields any matches to the input as `AurInfo`, but
-- doesn't include dependency information.
search :: Text -> IO [AurInfo]
search = undefined

-- | Returns all information about one package.
info :: Text -> IO (Maybe AurInfo)
info = undefined
--info pkg = listToMaybe <$> multiinfo [pkg]

-- | Like `info`, but can handle requests for multiple packages at once.
-- More efficient than using `info` multiple times.
--multiinfo :: [Text] -> IO [AurInfo]
multiinfo pkgs = (pretty <$>) <$> fetch pkgs "multiinfo" "arg[]"

-- | Search the AUR by Maintainer name.
msearch :: Text -> IO [AurInfo]
msearch = undefined

-- Seems we can convert the reponse to the JSON we need right away.
fetch :: [Text] -> String -> Text -> IO (Maybe Value)
fetch pkgs method argLabel = rpcResults <$> (getWith opts $ rpcUrl method)
    where opts = defaults & param argLabel .~ pkgs

rpcResults :: AsValue r => Response r -> Maybe Value
rpcResults r = r ^? responseBody . key "results"

-- | Conversion of JSON to nicely formated text.
pretty :: Value -> Text
pretty = decodeUtf8 . encodePretty
