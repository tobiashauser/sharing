{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad
import Language.Javascript.JSaddle 
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
subscribe :: MisoString -> (JSVal -> JSM result) -> (result -> action) -> Sub action
subscribe = subscribeWithOptions defaultOptions

-- | A generic version of @windowSubWithOptions that doesn't require a pure
-- @Decoder.
-- 
-- You are responsible to check for undefined return values (e.g. when working
-- with @JSVal). This can be done by returning a @Maybe and checking for null or
-- undefined with @isNull.
subscribeWithOptions :: Options -> MisoString -> (JSVal -> JSM result) -> (result -> action) -> Sub action
subscribeWithOptions Options{..} name handle action sink = createSub aquire release sink
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

handleDragEvent :: JSVal -> JSM Double
handleDragEvent dragEvent = do
  dataTransfer <- dragEvent ! "dataTransfer"
  dataTransferItemList <- dataTransfer ! "items"
  -- Return (change to actual value after transformation pipeline is done)
  now

data Item = File
          | Folder { items :: [ Item ] }


-- data DataTransfer =

-- dataTransferDecoder :: Decoder DataTransfer
