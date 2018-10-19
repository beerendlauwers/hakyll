--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Provider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (unless)
import qualified Data.Text as T
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Internal
import           Hakyll.Core.Provider.Metadata
import qualified Hakyll.Core.Store             as Store


--------------------------------------------------------------------------------
resourceMetadata :: Provider -> Identifier -> IO Metadata
resourceMetadata p r
    | not (resourceExists p r) = return mempty
    | otherwise                = do
        -- TODO keep time in md cache
        load p r
        Store.Found (BinaryMetadata md) <- Store.get (providerStore p)
            [name, T.pack $ toFilePath r, "metadata"]
        return md


--------------------------------------------------------------------------------
resourceBody :: Provider -> Identifier -> IO T.Text
resourceBody p r = do
    load p r
    Store.Found bd <- Store.get (providerStore p)
        [name, T.pack $ toFilePath r, "body"]
    maybe (resourceString p r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: Provider -> Identifier -> IO ()
resourceInvalidateMetadataCache p r = do
    Store.delete (providerStore p) [name, T.pack $ toFilePath r, "metadata"]
    Store.delete (providerStore p) [name, T.pack $ toFilePath r, "body"]


--------------------------------------------------------------------------------
load :: Provider -> Identifier -> IO ()
load p r = do
    mmof <- Store.isMember store mdk
    unless mmof $ do
        (md, body) <- loadMetadata p r
        Store.set store mdk (BinaryMetadata md)
        Store.set store bk  body
  where
    store = providerStore p
    mdk   = [name, T.pack $ toFilePath r, "metadata"]
    bk    = [name, T.pack $ toFilePath r, "body"]


--------------------------------------------------------------------------------
name :: T.Text
name = "Hakyll.Core.Resource.Provider.MetadataCache"
