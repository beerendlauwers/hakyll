{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings          #-}

--------------------------------------------------------------------------------
-- | A Module that allows easy rendering of RSS feeds.
--
-- The main rendering functions (@renderRss@, @renderAtom@) all assume that
-- you pass the list of items so that the most recent entry in the feed is the
-- first item in the list.
--
-- Also note that the context should have (at least) the following fields to
-- produce a correct feed:
--
-- - @$title$@: Title of the item
--
-- - @$description$@: Description to appear in the feed
--
-- - @$url$@: URL to the item - this is usually set automatically.
--
-- In addition, the posts should be named according to the rules for
-- 'Hakyll.Web.Template.Context.dateField'
module Hakyll.Web.Feed
    ( FeedConfiguration (..)
    , renderRss
    , renderAtom
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.Util.String     (replaceAll)
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Template.List


--------------------------------------------------------------------------------
import           Data.FileEmbed              (makeRelativeToProject, embedFile)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T


--------------------------------------------------------------------------------
rssTemplate :: T.Text
rssTemplate =
    T.decodeUtf8 $(makeRelativeToProject "data/templates/rss.xml" >>= embedFile)

rssItemTemplate :: T.Text
rssItemTemplate =
    T.decodeUtf8 $(makeRelativeToProject "data/templates/rss-item.xml" >>= embedFile)

atomTemplate :: T.Text
atomTemplate =
  T.decodeUtf8 $(makeRelativeToProject "data/templates/atom.xml" >>= embedFile)

atomItemTemplate :: T.Text
atomItemTemplate =
    T.decodeUtf8 $(makeRelativeToProject "data/templates/atom-item.xml" >>= embedFile)

--------------------------------------------------------------------------------
-- | This is a data structure to keep the configuration of a feed.
data FeedConfiguration = FeedConfiguration
    { -- | Title of the feed.
      feedTitle       :: String
    , -- | Description of the feed.
      feedDescription :: String
    , -- | Name of the feed author.
      feedAuthorName  :: String
    , -- | Email of the feed author.
      feedAuthorEmail :: String
    , -- | Absolute root URL of the feed site (e.g. @http://jaspervdj.be@)
      feedRoot        :: String
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Abstract function to render any feed.
renderFeed :: T.Text                  -- ^ Default feed template
           -> T.Text                  -- ^ Default item template
           -> FeedConfiguration       -- ^ Feed configuration
           -> Context T.Text          -- ^ Context for the items
           -> [Item T.Text]           -- ^ Input items
           -> Compiler (Item T.Text)  -- ^ Resulting item
renderFeed defFeed defItem config itemContext items = do
    feedTpl <- readTemplateFile defFeed
    itemTpl <- readTemplateFile defItem

    protectedItems <- mapM (applyFilter protectCDATA) items
    body <- makeItem =<< applyTemplateList itemTpl itemContext' protectedItems
    applyTemplate feedTpl feedContext body
  where
    applyFilter :: (Monad m,Functor f) => (a -> a) -> f a -> m (f a)
    applyFilter tr str = return $ fmap tr str
    protectCDATA :: T.Text -> T.Text
    protectCDATA = T.pack . replaceAll "]]>" (const "]]&gt;") . T.unpack

    itemContext' = mconcat
        [ itemContext
        , constField "root" (T.pack $ feedRoot config)
        , constField "authorName"  (T.pack $ feedAuthorName config)
        , constField "authorEmail" (T.pack $ feedAuthorEmail config)
        ]

    feedContext = mconcat
         [ bodyField  "body"
         , constField "title"       (T.pack $ feedTitle config)
         , constField "description" (T.pack $ feedDescription config)
         , constField "authorName"  (T.pack $ feedAuthorName config)
         , constField "authorEmail" (T.pack $ feedAuthorEmail config)
         , constField "root"        (T.pack $ feedRoot config)
         , urlField   "url"
         , updatedField
         , missingField
         ]

    -- Take the first "updated" field from all items -- this should be the most
    -- recent.
    updatedField = field "updated" $ \_ -> case items of
        []      -> return "Unknown"
        (x : _) -> unContext itemContext' "updated" [] x >>= \cf -> case cf of
            ListField _ _ -> fail "Hakyll.Web.Feed.renderFeed: Internal error"
            StringField s -> return s

    readTemplateFile :: T.Text -> Compiler Template
    readTemplateFile value = pure $ template $ readTemplateElems value


--------------------------------------------------------------------------------
-- | Render an RSS feed with a number of items.
renderRss :: FeedConfiguration       -- ^ Feed configuration
          -> Context T.Text          -- ^ Item context
          -> [Item T.Text]           -- ^ Feed items
          -> Compiler (Item T.Text)  -- ^ Resulting feed
renderRss config context = renderFeed
    rssTemplate rssItemTemplate config
    (makeItemContext "%a, %d %b %Y %H:%M:%S UT" context)


--------------------------------------------------------------------------------
-- | Render an Atom feed with a number of items.
renderAtom :: FeedConfiguration       -- ^ Feed configuration
           -> Context T.Text          -- ^ Item context
           -> [Item T.Text]           -- ^ Feed items
           -> Compiler (Item T.Text)  -- ^ Resulting feed
renderAtom config context = renderFeed
    atomTemplate atomItemTemplate config
    (makeItemContext "%Y-%m-%dT%H:%M:%SZ" context)


--------------------------------------------------------------------------------
-- | Copies @$updated$@ from @$published$@ if it is not already set.
makeItemContext :: String -> Context a -> Context a
makeItemContext fmt context = mconcat
    [dateField "published" fmt, context, dateField "updated" fmt]
