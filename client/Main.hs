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

-- | Model
data Model = Model { _dragging :: Bool
                   , _lastDrag :: Double
                   } deriving (Eq, Show)

initialModel :: Model
initialModel = Model { _dragging = False
                     , _lastDrag = 0
                     }

-- | Lenses

dragging :: Lens Model Bool
dragging = lens _dragging $ \record field -> record { _dragging = field }

lastDrag :: Lens Model Double
lastDrag = lens _lastDrag $ \record field -> record { _lastDrag = field }

-- | Action
data Action = DragEnter
            | DragLeave
            | HasDragLeft
            | LastDrag Double

-- | Subscriptions

dragEnterSub :: Sub Action
dragEnterSub = windowSub "dragenter" emptyDecoder $ \_ -> DragEnter

dragLeaveSub :: Sub Action
dragLeaveSub = windowSub "dragleave" emptyDecoder $ \_ -> DragLeave

-- | Reducer
updateModel :: Action -> Transition Model Action
updateModel DragEnter = do
  dragging .= True
  io $ (<$>) LastDrag now
updateModel DragLeave = delay 2000 $ HasDragLeft
updateModel (LastDrag time) = lastDrag .= time

-- | View
viewModel :: Model -> View Model Action
viewModel state = p_ [] [ text $ "LastDrag: " <> ms (_lastDrag state) ]

-- | Main
app :: App Model Action
app = (component initialModel updateModel viewModel) {
  subs = [ dragEnterSub, dragLeaveSub ]
  }

main :: IO () 
main = run $ startApp app
