module Dragging where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.Query.Event (eventListener)
import Halogen.Subscription (Emitter, create)
import Web.Clipboard.ClipboardEvent (toEvent)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget)
import Web.HTML (window)
import Web.HTML.Event.DragEvent.EventTypes (dragenter)
import Web.HTML.Window (toEventTarget)

-------------------------------------------------------------------------------
--- Subscriptions
-------------------------------------------------------------------------------

-- onDragenter :: forall action. EventTarget -> (Event -> Maybe action) -> Emitter action 
-- onDragenter target toAction = eventListener dragenter target toAction

blob :: forall m a. MonadAff m => (Event -> a) -> m (Emitter a)
blob toAction = do
  { emitter, listener } <- liftEffect create
  win                   <- liftEffect window
  eventListener (toEventTarget win) (\event -> toAction event)
