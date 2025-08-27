{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Lens
import Lib
import Miso
import Miso.String
import Miso.Event.Types
import Language.Javascript.JSaddle.Value
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Object

-------------------------------------------------------------------------------
--- Model           
-------------------------------------------------------------------------------

data Model = Model { _isDragging :: Bool
                   , _dragTimes :: (Double, Double)  -- (dragenter, dragleave)
                   } deriving (Eq, Show)

-- | Generate van Laarhoven lenses.
$(makeLenses ''Model)

-- | The initial state of the application.
initialModel :: Model
initialModel = Model { _isDragging = False
                     , _dragTimes = (0, 0)
                     }

-------------------------------------------------------------------------------
--- Actions         
-------------------------------------------------------------------------------

data Action = None
            | DragEnter
            | DragLeave
            | DragLeft
            | Drop (Maybe JSVal)
            | forall a. Loggable a => Log a
            | forall a. Set (a -> Model -> Model) a

-------------------------------------------------------------------------------
--- Reducer         
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel DragEnter = do
  isDragging .= True
  io $ Set (dragTimes . _1 .~) <$> now

updateModel DragLeave = do
  Lib.delay 200 DragLeft
  io $ Set (dragTimes . _2 .~) <$> now

updateModel DragLeft = do
  (lhs, rhs) <- use dragTimes
  for $ pure $ inCase ((lhs + 50) < rhs) $ Set (isDragging .~) False
  -- Think of the +50 as a delay to give the browser engine a chance to call the
  -- next event.
  
updateModel (Drop (Just a)) = io_ $ consoleLog' a
updateModel (Drop Nothing)  = io_ $ consoleLog "nothing dropped"

updateModel (Set set a) = modify $ set a

updateModel (Log a) = io_ $ _log a 

updateModel _ = noop None 

-------------------------------------------------------------------------------
--- Subscriptions   
-------------------------------------------------------------------------------

dragOptions :: Options
dragOptions = Options { preventDefault = True
                     , stopPropagation = True }

dragEnter :: Sub Action
dragEnter = windowSubWithOptions dragOptions "dragenter" emptyDecoder $ \_ -> DragEnter

dragLeave :: Sub Action
dragLeave = windowSubWithOptions dragOptions "dragleave" emptyDecoder $ \_ -> DragLeave

-- See https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/drop_event
-- why we need the options.
dragOver :: Sub Action
-- dragOver = windowSubWithOptions subOptions "dragover" emptyDecoder $ \_ -> Log $ ms "dragover"
dragOver = windowSubscribe "dragover"
  (\e -> do
      e ^. js0 "preventDefault"
      e ^. js0 "stopPropagation"
      return e)
  (\e -> None)
  
drop :: Sub Action
-- drop = windowSubscribeWithOptions subOptions "drop" return $ \e -> Drop (Just e)
drop = windowSubscribe "drop"
  (\e -> do
     e ^. js0 "preventDefault"
     e ^. js0 "stopPropagation"
     return e)
  (\e -> Drop $ Just e)
  
-------------------------------------------------------------------------------
--- View            
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel state = div_ []
  [ p_ [] [ text $ "Dragging: " <> (ms (show (state ^. isDragging))) ]
  ]

-------------------------------------------------------------------------------
--- App             
-------------------------------------------------------------------------------

app :: App Model Action
app = (component initialModel updateModel viewModel) {
  subs = [ dragEnter
         , dragLeave
         , dragOver
         , Main.drop ]
  }

main :: IO () 
main = run $ startApp app
