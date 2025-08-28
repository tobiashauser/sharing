{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar, takeMVar)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(..))
import JavaScript.Array (toListIO)
import Language.Javascript.JSaddle hiding (isNull, isUndefined)
import Language.Javascript.JSaddle.Native.Internal (isNull, isUndefined)
import Language.Javascript.JSaddle.Value hiding (isNull, isUndefined)
import Miso
import Miso.FFI
import Miso.String
import Miso.Subscription.Util

-------------------------------------------------------------------------------
--- Miso Internals  
-------------------------------------------------------------------------------

-- These are redefinitions of functions that are internal in miso but to which I
-- need access to.

windowAddEventListener :: MisoString -> (JSVal -> JSM ()) -> JSM Function
windowAddEventListener name cb = do
  win <- jsg "window"
  addEventListener win name cb

removeEventListener :: JSVal -> MisoString -> Function -> JSM ()
removeEventListener self name cb = void $ self # "removeEventListener" $ (name, cb)

windowRemoveEventListener :: MisoString -> Function -> JSM ()
windowRemoveEventListener name cb = do
  win <- jsg "window"
  removeEventListener win name cb

eventPreventDefault :: JSVal -> JSM ()
eventPreventDefault e = do
  _ <- e # "preventDefault" $ ()
  pure ()

eventStopPropagation :: JSVal -> JSM ()
eventStopPropagation e = do
  _ <- e # "stopPropagation" $ ()
  pure ()

-------------------------------------------------------------------------------
--- Extensions      
-------------------------------------------------------------------------------

-- instance ToMisoString a => ToMisoString (Maybe a) where
--   toMisoString Nothing = "[null]"
--   toMisoString (Just x) = toMisoString x

class Loggable a where
  _log :: a -> JSM ()

instance Loggable JSVal where
  _log val = consoleLog' val

instance Loggable [JSVal] where
  _log vals = mapM_ _log vals

instance Loggable MisoString where
  _log = consoleLog

instance Loggable a => Loggable (Maybe a) where
  _log (Just val) = _log val
  _log Nothing = _log $ ms "[null]"

instance Loggable a => Loggable [a] where
  _log as = mapM_ _log as

valToMs :: ToJSVal value => value -> JSM MisoString
valToMs value = do
  text <- valToText value
  return $ toMisoString text

-------------------------------------------------------------------------------
--- Actions         
-------------------------------------------------------------------------------

-- | Return an @action@ to the update function after a delay @ms@.
--
-- > updateModel _ = delay 1000 SayHello
delay :: Int -> action -> Effect parent model action
delay ms action = withSink $ \sink -> do
  win <- jsg "window"
  cb  <- asyncCallback $ sink action
  void $ win # "setTimeout" $ (cb, ms)

-- | Optionally return a result if a condition is met.
--
-- > updateModel _ = for $ pure $ inCase (1 < 2) SayHello
inCase :: Bool -> result -> Maybe result
inCase predicate result
  | predicate = Just result
  | otherwise = Nothing

-- Is this equivalent to @when?

-------------------------------------------------------------------------------
--- Subscriptions    
-------------------------------------------------------------------------------

-- | A generic version of @windowSub that doesn't require a pure @Decoder.
--
-- You are responsible to check for undefined return values (e.g. when working
-- with @JSVal). This can be done by returning a @Maybe and checking for null or
-- undefined with @isNull.
windowSubscribe :: MisoString -> (JSVal -> JSM result) -> (result -> action) -> Sub action
windowSubscribe = windowSubscribeWithOptions defaultOptions

-- | A generic version of @windowSubWithOptions that doesn't require a pure
-- @Decoder.
-- 
-- You are responsible to check for undefined return values (e.g. when working
-- with @JSVal). This can be done by returning a @Maybe and checking for null or
-- undefined with @isNull.
windowSubscribeWithOptions :: Options -> MisoString -> (JSVal -> JSM result) -> (result -> action) -> Sub action
windowSubscribeWithOptions Options{..} name handle action sink = createSub aquire release sink
  where
    release = windowRemoveEventListener name
    aquire = windowAddEventListener name $ \event -> do
      when preventDefault $ eventPreventDefault event   -- apply the options 
      when stopPropagation $ eventStopPropagation event -- before running the 
      result <- handle event                            -- handle function
      sink (action result)

-------------------------------------------------------------------------------
--- Files and Folders
-------------------------------------------------------------------------------

--- Item

data Item = File   { name :: MisoString
                   , file :: Maybe JSVal
                   , fileSystemFileEntry :: JSVal }
          | Folder { name :: MisoString
                   , contents :: [Item]
                   , fileSystemDirectoryEntry :: JSVal }

instance Loggable Item where
  _log (File name file fileSystemFileEntry) = do
    _log $ "File { name = " <> name <> " }"
    _log file
    _log fileSystemFileEntry
  _log (Folder name contents fileSystemDirectoryEntry) = do
    _log $ "Folder { name = " <> name <> " }"
    _log contents
    _log fileSystemDirectoryEntry

--- Handlers

handleDragEvent :: JSVal -> JSM (Maybe [Item])
handleDragEvent dragEvent = runMaybeT $ do
  dataTransfer         <- ensure $ dragEvent ! "dataTransfer"
  dataTransferItemList <- ensure $ dataTransfer ! "items"
  dataTransferItems    <- lift $ toListIO $ SomeJSArray dataTransferItemList
  fileSystemEntries    <- lift $ mapM toFileSystemEntry dataTransferItems
  items                <- lift $ mapM toItem fileSystemEntries
  return items
  where
    ensure :: JSM JSVal -> MaybeT JSM JSVal
    ensure m = MaybeT $ do
      v <- m
      n <- isNull v
      u <- isUndefined v
      pure $ if n || u then Nothing else Just v

    toFileSystemEntry :: JSVal -> JSM JSVal
    toFileSystemEntry v = v # "webkitGetAsEntry" $ ()

    toItem :: JSVal -> JSM Item
    toItem v = do
      isDirectory <- v ! "isDirectory" >>= valToBool
      case isDirectory of
        -- File
        False -> do
          name <- v ! "name" >>= valToMs
          file <- getFile v
          return $ File { name = name
                        , file = file
                        , fileSystemFileEntry = v }
        -- Folder
        True -> do
          name <- v ! "name" >>= valToMs
          return $ Folder { name = name
                          , contents = []
                          , fileSystemDirectoryEntry = v }

    -- | Input: FileSystemFileEntry
    getFile :: JSVal -> JSM (Maybe JSVal)
    getFile e = do
      result <- liftIO newEmptyMVar
      e # "file" $ (success result, error result)
      liftIO $ takeMVar result
        where
          success result = syncCallback1 $ \file -> do
            liftIO $ void $ tryPutMVar result $ Just file
          error result = syncCallback1 $ \_ -> do
            liftIO $ void $ tryPutMVar result Nothing

    -- | Input: FileSystemDirectoryEntry
    -- getContents :: JSVal -> JSM [Item]
    -- getContents e = do
      
    

