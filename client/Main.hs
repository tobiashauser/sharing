{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Lens
import Lib
import Miso
-- import Miso.String
-- import Miso.Event.Types
import Control.Monad (forM_)

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
            | Drop (Maybe [Item])
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
  
updateModel (Drop (Just items)) = withSink $ \sink -> do
  forM_ items $ \item -> do
    file <- getFile item 
    sink (Log file)

updateModel (Drop Nothing)  =
  io_ $ consoleError "An error occured in `handleDragEvent': got nothing."

updateModel (Set set a) = modify $ set a

updateModel (Log a) = io_ $ _log a 

updateModel _ = noop None

-------------------------------------------------------------------------------
--- Subscriptions   
-------------------------------------------------------------------------------

-- See https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/drop_event
-- why we need these options.
dragOptions :: Options
dragOptions = Options { preventDefault = True
                     , stopPropagation = True }

dragEnter :: Sub Action
dragEnter = windowSubWithOptions dragOptions "dragenter" emptyDecoder $ \_ -> DragEnter

dragLeave :: Sub Action
dragLeave = windowSubWithOptions dragOptions "dragleave" emptyDecoder $ \_ -> DragLeave

dragOver :: Sub Action
dragOver = windowSubWithOptions dragOptions "dragover" emptyDecoder $ \_ -> None

-- There is a bug in the way the options are applied when running the website
-- with JSaddle: The drop event is not fired because the default action is not
-- prevented. Use the WASM compile target instead.
drop :: Sub Action
drop = windowSubscribeWithOptions dragOptions "drop" handleDragEvent Drop
  
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

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

app :: App Model Action
app = (component initialModel updateModel viewModel) {
  subs = [ dragEnter
         , dragLeave
         , dragOver
         , Main.drop ]
  }

main :: IO () 
main = run $ startApp app

