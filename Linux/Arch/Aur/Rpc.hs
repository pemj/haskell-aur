{-# LANGUAGE OverloadedStrings #-}

module Linux.Arch.Aur.Rpc
       ( -- * Queries
         search
       , info
       , multiinfo
       , msearch
       -- * As JSON
       , search'
       , info'
       , multiinfo'
       , msearch'
       -- * Pretty JSON
       , pretty) where

import Linux.Arch.Aur.Types

import Control.Applicative  ((<$>))
import Control.Monad.Trans  (MonadIO, liftIO)
import Control.Lens
import Data.Aeson           (Value(..), FromJSON, Result(..), fromJSON)
import Data.Aeson.Lens      (AsValue, _String, key, nth)
import Data.Aeson.Encode.Pretty
import Data.Map.Lazy
import Data.Maybe           (catMaybes, listToMaybe, maybeToList, fromJust)
import Data.Text
import Data.Text.Lazy.Encoding
import Network.Wreq

import qualified Data.List      as L
import qualified Data.Text.Lazy as TL
import qualified Data.Vector    as V

---

rpcUrl :: String
rpcUrl = "https://aur.archlinux.org/rpc.php?"

apiVersion :: Text
apiVersion = "2"

-- | Yields any matches to the input as `AurInfo`, but
-- doesn't include dependency information.
search :: (MonadIO m, Functor m) => Text -> m [AurInfo]
search s = mapArray <$> search' s

-- | `search` call as Haskellised JSON.
search' :: MonadIO m => Text -> m (Maybe Value)
search' query = rpc "search" [query] "arg"

-- | Returns all information about one package.
info :: (MonadIO m, Functor m) => Text -> m (Maybe AurInfo)
info p = (>>= extract) <$> info' p

-- | `info` call as Haskellised JSON.
info' :: (MonadIO m, Functor m) => Text -> m (Maybe Value)
info' pkg = (>>= (^? nth 0)) <$> multiinfo' [pkg]

-- | Like `info`, but can handle requests for multiple packages at once.
-- More efficient than using `info` multiple times.
multiinfo :: (MonadIO m, Functor m) => [Text] -> m [AurInfo]
multiinfo p = mapArray <$> multiinfo' p

-- | `multiinfo` call as Haskellised JSON.
multiinfo' :: MonadIO m => [Text] -> m (Maybe Value)
multiinfo' pkgs = rpc "multiinfo" pkgs "arg[]"

-- | Search the AUR by Maintainer name.
msearch :: (MonadIO m, Functor m) => Text -> m [AurInfo]
msearch m = mapArray <$> msearch' m

-- | `msearch` call as Haskellised JSON.
msearch' :: MonadIO m => Text -> m (Maybe Value)
msearch' maintainer = rpc "msearch" [maintainer] "arg"

-- | Call the RPC.
-- Doesn't fail elegantly when there is a connection failure.
rpc :: MonadIO m => Text -> [Text] -> Text -> m (Maybe Value)
rpc method args argLabel = liftIO (rpcResults <$> getWith opts rpcUrl)
    where opts = defaults & param "type"   .~ [method]
                          & param argLabel .~ args
                          & param "v"      .~ [apiVersion]

rpcResults :: AsValue r => Response r -> Maybe Value
rpcResults r = r ^? responseBody . key "results"

-- | Conversion of JSON to nicely formated text.
pretty :: Value -> Text
pretty = TL.toStrict . decodeUtf8 . encodePretty

extract :: FromJSON a => Value -> Maybe a
extract = f . fromJSON
    where f (Success x) = Just x
          f _           = Nothing

maybeToList' :: Maybe [a] -> [a]
maybeToList' = L.concat . maybeToList

-- There must be a better way to do this.
mapArray :: FromJSON a => Maybe Value -> [a]
mapArray (Just (Array v)) = catMaybes . V.toList . V.map extract $ v
mapArray _                = []
