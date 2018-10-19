--------------------------------------------------------------------------------
-- | Describes writable items; items that can be saved to the disk
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Hakyll.Core.Writable
    ( Writable (..)
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString                 as SB
import qualified Data.ByteString.Lazy            as LB
import           Data.Word                       (Word8)
import           Text.Blaze.Html                 (Html)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------
import           Hakyll.Core.Item


--------------------------------------------------------------------------------
-- | Describes an item that can be saved to the disk
class Writable a where
    -- | Save an item to the given filepath
    write :: FilePath -> Item a -> IO ()

instance Writable T.Text where
    write p = TIO.writeFile p . itemBody

--------------------------------------------------------------------------------
instance Writable () where
    write _ _ = return ()


--------------------------------------------------------------------------------
instance Writable [Char] where
    write p = writeFile p . itemBody


--------------------------------------------------------------------------------
instance Writable SB.ByteString where
    write p = SB.writeFile p . itemBody


--------------------------------------------------------------------------------
instance Writable LB.ByteString where
    write p = LB.writeFile p . itemBody


--------------------------------------------------------------------------------
instance Writable [Word8] where
    write p = write p . fmap SB.pack


--------------------------------------------------------------------------------
instance Writable Html where
    write p = write p . fmap renderHtml
