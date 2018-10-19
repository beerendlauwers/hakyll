--------------------------------------------------------------------------------
-- | This module exposes a function which can relativize URL's on a webpage.
--
-- This means that one can deploy the resulting site on
-- @http:\/\/example.com\/@, but also on @http:\/\/example.com\/some-folder\/@
-- without having to change anything (simply copy over the files).
--
-- To use it, you should use absolute URL's from the site root everywhere. For
-- example, use
--
-- > <img src="/images/lolcat.png" alt="Funny zomgroflcopter" />
--
-- in a blogpost. When running this through the relativize URL's module, this
-- will result in (suppose your blogpost is located at @\/posts\/foo.html@:
--
-- > <img src="../images/lolcat.png" alt="Funny zomgroflcopter" />
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Web.Html.RelativizeUrls
    ( relativizeUrls
    , relativizeUrlsWith
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text as T


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
-- | Compiler form of 'relativizeUrls' which automatically picks the right root
-- path
relativizeUrls :: Item T.Text -> Compiler (Item T.Text)
relativizeUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (relativizeUrlsWith $ T.pack $ toSiteRoot r) item


--------------------------------------------------------------------------------
-- | Relativize URL's in HTML
relativizeUrlsWith :: T.Text  -- ^ Path to the site root
                   -> T.Text  -- ^ HTML to relativize
                   -> T.Text  -- ^ Resulting HTML
relativizeUrlsWith root = withUrls rel
  where
    isRel x = "/" ` T.isPrefixOf` x && not ("//" `T.isPrefixOf` x)
    rel x   = if isRel x then root `T.append` x else x
