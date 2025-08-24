{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle (jsg, (#))
import Control.Monad (void)
import Miso

delay :: Int -> action -> Transition model action
delay ms action = withSink $ \sink -> do
  win <- jsg ("window" :: MisoString)
  cb  <- asyncCallback $ sink action
  void $ win # ("setTimeout" :: MisoString) $ (cb, ms)

-- | Model
type Model = ()

-- | Action
data Action = Hello { name :: String } 
            | DelayedHello { name :: String }
            
-- | Reducer
updateModel :: Action -> Transition Model Action
updateModel (Hello name) = io_ $ consoleLog $ "Hello, " <> (ms name) <> "!"
updateModel (DelayedHello name) = delay 3000 $ Hello name

-- | View
viewModel :: Model -> View Model Action
viewModel () = p_ [] [ text "Hello, client!" ]

-- | Main
app :: App Model Action
app = (component () updateModel viewModel) {
  initialAction = Just $ DelayedHello "Mr. Blob"
  }

main :: IO () 
main = run $ startApp app
