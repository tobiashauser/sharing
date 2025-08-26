{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Lens
import Lib
import Miso

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
            | forall a. Set (a -> Model -> Model) a

-------------------------------------------------------------------------------
--- Reducer         
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel DragEnter = do
  isDragging .= True
  io $ Set (dragTimes . _1 .~) <$> now

updateModel DragLeave = do
  delay 200 DragLeft
  io $ Set (dragTimes . _2 .~) <$> now

updateModel DragLeft = do
  (lhs, rhs) <- use dragTimes
  for $ pure $ inCase ((lhs + 50) < rhs) $ Set (isDragging .~) False
  -- Think of the +50 as a delay to give the browser engine a chance to call the
  -- nexte event.

updateModel (Set set a) = modify $ set a

updateModel _ = noop None 

-------------------------------------------------------------------------------
--- Subscriptions   
-------------------------------------------------------------------------------

dragEnter :: Sub Action
dragEnter = windowSub "dragenter" emptyDecoder $ \_ -> DragEnter

dragLeave :: Sub Action
dragLeave = windowSub "dragleave" emptyDecoder $ \_ -> DragLeave

-- See https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/drop_event
-- why we need the options.
dragOver :: Sub Action
dragOver = windowSubWithOptions Options { preventDefault = True, stopPropagation = True }
  "dragover" emptyDecoder $ \_ -> None

-- CONTINUE HERE
-- drop :: Sub Action
-- drop = windowSub "drop" 
  
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
         , dragOver ]
  }

main :: IO () 
main = run $ startApp app
