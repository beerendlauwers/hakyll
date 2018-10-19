--------------------------------------------------------------------------------
-- | Produce pretty, thread-safe logs
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Logger
    ( Verbosity (..)
    , Logger
    , new
    , flush
    , error
    , header
    , message
    , debug
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Control.Monad.Trans     (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Prelude                 hiding (error)


--------------------------------------------------------------------------------
data Verbosity
    = Error
    | Message
    | Debug
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- | Logger structure. Very complicated.
data Logger = Logger
    { loggerChan      :: Chan (Maybe T.Text)  -- ^ Nothing marks the end
    , loggerSync      :: MVar ()              -- ^ Used for sync on quit
    , loggerSink      :: T.Text -> IO ()      -- ^ Out sink
    , loggerVerbosity :: Verbosity            -- ^ Verbosity
    }


--------------------------------------------------------------------------------
-- | Create a new logger
new :: Verbosity -> IO Logger
new vbty = do
    logger <- Logger <$>
        newChan <*> newEmptyMVar <*> pure TIO.putStrLn <*> pure vbty
    _      <- forkIO $ loggerThread logger
    return logger
  where
    loggerThread logger = forever $ do
        msg <- readChan $ loggerChan logger
        case msg of
            -- Stop: sync
            Nothing -> putMVar (loggerSync logger) ()
            -- Print and continue
            Just m  -> loggerSink logger m


--------------------------------------------------------------------------------
-- | Flush the logger (blocks until flushed)
flush :: Logger -> IO ()
flush logger = do
    writeChan (loggerChan logger) Nothing
    () <- takeMVar $ loggerSync logger
    return ()


--------------------------------------------------------------------------------
string :: MonadIO m
       => Logger     -- ^ Logger
       -> Verbosity  -- ^ Verbosity of the string
       -> T.Text     -- ^ Section name
       -> m ()       -- ^ No result
string l v m
    | loggerVerbosity l >= v = liftIO $ writeChan (loggerChan l) (Just m)
    | otherwise              = return ()


--------------------------------------------------------------------------------
error :: MonadIO m => Logger -> T.Text -> m ()
error l m = string l Error $ "  [ERROR] " `T.append` m


--------------------------------------------------------------------------------
header :: MonadIO m => Logger -> T.Text -> m ()
header l = string l Message


--------------------------------------------------------------------------------
message :: MonadIO m => Logger -> T.Text -> m ()
message l m = string l Message $ "  " `T.append` m


--------------------------------------------------------------------------------
debug :: MonadIO m => Logger -> T.Text -> m ()
debug l m = string l Debug $ "  [DEBUG] " `T.append` m
