--------------------------------------------------------------------------------
-- | Parser utilities
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}
module Hakyll.Core.Util.Parser
    ( metadataKey
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Control.Monad       (guard, mzero, void)
import qualified Data.Text           as T
import qualified Text.Parsec         as P
import           Text.Parsec.Text    (Parser)


--------------------------------------------------------------------------------
metadataKey :: Parser T.Text
metadataKey = do
    -- Ensure trailing '-' binds to '$' if present.
    let hyphon = P.try $ do
            void $ P.char '-'
            x <- P.lookAhead P.anyChar
            guard $ x /= '$'
            pure '-'

    (T.pack -> i) <- (:) <$> P.letter <*> P.many (P.alphaNum <|> P.oneOf "_." <|> hyphon)
    if i `elem` reservedKeys then mzero else return i


--------------------------------------------------------------------------------
reservedKeys :: [T.Text]
reservedKeys = ["if", "else", "endif", "for", "sep", "endfor", "partial"]
