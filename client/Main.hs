{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle 
import Control.Monad 
import Miso
import Miso.Lens

delay :: Int -> action -> Transition model action
delay ms action = withSink $ \sink -> do
  win <- jsg ("window" :: MisoString)
  cb  <- asyncCallback $ sink action
  void $ win # ("setTimeout" :: MisoString) $ (cb, ms)

inCase :: Ord a => a -> (a -> a -> Bool) -> a -> result -> Maybe result
inCase lhs compare rhs success
  | compare lhs rhs = Just success
  | otherwise = Nothing

-- | Model
data Model = Model { _dragging :: Bool
                   , _dragEnteredAt :: Double
                   , _dragLeftAt :: Double
                   } deriving (Eq, Show)

initialModel :: Model
initialModel = Model { _dragging = False
                     , _dragEnteredAt = 0
                     , _dragLeftAt = 0
                     }

-- | Lenses
dragging :: Lens Model Bool
dragging = lens _dragging $ \record field -> record { _dragging = field }

dragEnteredAt :: Lens Model Double
dragEnteredAt = lens _dragEnteredAt $ \record field -> record { _dragEnteredAt = field }

dragLeftAt :: Lens Model Double
dragLeftAt = lens _dragLeftAt $ \record field -> record { _dragLeftAt = field }

-- | Action
data Action = DragEnter
            | DragLeave Bool -- if False delay 
            | DelayDragLeave
            | SetDragging Bool
            | SetDragEnteredAt Double
            | SetDragLeftAt Double

-- | Subscriptions

dragEnter :: Sub Action
dragEnter = windowSub "dragenter" emptyDecoder $ \_ -> DragEnter

dragLeave :: Sub Action
dragLeave = windowSub "dragleave" emptyDecoder $ \_ -> DragLeave

-- | Reducer
updateModel :: Action -> Transition Model Action

updateModel DragEnter = do
  io_ $ consoleLog "DragEnter"
  batch [
    pure $ SetDragging True
    , SetDragEnteredAt <$> now
    ]

updateModel (DragLeave False) = do
  delay 2000 $ DragLeave True
  io $ SetDragLeftAt <$> now

updateModel (DragLeave True) = do
  dragEnteredAt <- gets _dragEnteredAt
  dragLeftAt <- gets _dragLeftAt
  for blob 
  -- for $ inCase dragEnteredAt (<) dragLeftAt $ SetDragging <*> pure False
  -- for $ inCase lastDrag (<) <$> now <*> (pure $ SetDragging False)

updateModel (SetDragging bool) = dragging .= bool
updateModel (SetDragEnteredAt time) = dragEnteredAt .= time
updateModel (SetDragLeftAt time) = dragLeftAt .= time

blob = inCase 1 (<) 2 DragLeave <*> pure False

-- | View
viewModel :: Model -> View Model Action
viewModel state = p_ [] [
  text $ ms (show $ _dragEnteredAt state)
  , text $ ms (show $ _dragging state)
  ]

-- | Main
app :: App Model Action
app = (component initialModel updateModel viewModel) {
  subs = [ dragEnter, dragLeave ]
  , logLevel = DebugEvents
  }

main :: IO () 
main = run $ startApp app
