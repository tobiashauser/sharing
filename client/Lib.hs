{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Language.Javascript.JSaddle hiding (isNull, isUndefined)
import Language.Javascript.JSaddle.Value hiding (isNull, isUndefined)
import Language.Javascript.JSaddle.Native.Internal (isNull, isUndefined)
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

instance Loggable MisoString where
  _log = consoleLog 

instance Loggable a => Loggable (Maybe a) where
  _log (Just val) = _log val
  _log Nothing = _log $ ms "[null]"

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
      result <- handle event
      when preventDefault $ eventPreventDefault event
      when stopPropagation $ eventStopPropagation event
      sink (action result)

-------------------------------------------------------------------------------
--- Files and Folders
-------------------------------------------------------------------------------

handleDragEvent :: JSVal -> JSM (Maybe JSVal)
handleDragEvent dragEvent = runMaybeT $ do
  dataTransfer <- ensure $ dragEvent ! "dataTransfer"
  dataTransferItemList <- ensure $ dataTransfer ! "items"
  return dataTransferItemList
  where
    ensure :: JSM JSVal -> MaybeT JSM JSVal
    ensure m = MaybeT $ do
      v <- m
      n <- isNull v
      u <- isUndefined v
      pure $ if n || u then Nothing else Just v

data Item = File
          | Folder { items :: [ Item ] }


-- data DataTransfer =

-- dataTransferDecoder :: Decoder DataTransfer
