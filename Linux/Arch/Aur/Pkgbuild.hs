{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Linux.Arch.Aur.Pkgbuild
-- Copyright : (c) Colin Woodbury, 2014,2015
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Linux.Arch.Aur.Pkgbuild
    ( pkgbuild
    , pkgbuild'
    , pkgbuildUrl ) where

import           Control.Exception (SomeException, catch)
import           Control.Lens ((^?))
import           Control.Monad ((>=>))
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Text hiding (take)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding
import           Network.Wreq
import           System.FilePath ((</>))

---

type E = SomeException

baseUrl :: String
baseUrl = "https://aur4.archlinux.org/"

-- | The location of a given package's PKGBUILD on the AUR servers.
pkgbuildUrl :: String -> String
pkgbuildUrl p = baseUrl </> "cgit/aur.git/plain/PKGBUILD?h=" ++ p

-- | The PKGBUILD of a given package, retrieved from the AUR servers.
pkgbuild :: MonadIO m => String -> m (Maybe Text)
pkgbuild p = e $ (rb >=> txt) <$> get (pkgbuildUrl p)
    where rb  = (^? responseBody)
          txt = Just . TL.toStrict . decodeUtf8
          e f = liftIO $ f `catch` (\(_ :: E) -> return Nothing)

-- | Callable with Text as well, if that's easier.
pkgbuild' :: MonadIO m => Text -> m (Maybe Text)
pkgbuild' (unpack -> p) = pkgbuild p
