module Main where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (modify_, HalogenM, Component, ComponentHTML, mkComponent, mkEval, defaultEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (div_, button, text)
import Halogen.HTML.Events (onClick)
import Halogen.VDom.Driver (runUI)

-------------------------------------------------------------------------------
--- State
-------------------------------------------------------------------------------

type State = Int

initialState :: forall input. input -> State
initialState _ = 0

-------------------------------------------------------------------------------
--- Actions
-------------------------------------------------------------------------------

data Action = Increment | Decrement 

------------------------------------------------------------------------------- 
--- Subscriptions
-------------------------------------------------------------------------------

------------------------------------------------------------------------------- 
--- Reducer
-------------------------------------------------------------------------------

--- | Reduce function that transitions state.
handleAction :: forall output m. MonadAff m => Action -> HalogenM State Action () output m Unit
handleAction Increment = modify_ \state -> state + 1
handleAction Decrement = modify_ \state -> state - 1

------------------------------------------------------------------------------- 
--- Renderer
-------------------------------------------------------------------------------

render :: forall m. State -> ComponentHTML Action () m
render state =
  div_
    [ button [ onClick \_ -> Decrement ] [ text "-" ]
    , text (show state)
    , button [ onClick \_ -> Increment ] [ text "+" ]
    ]
    
------------------------------------------------------------------------------- 
--- App
-------------------------------------------------------------------------------

app :: forall query input output m. MonadAff m => Component query input output m
app = mkComponent { initialState
                  , render
                  , eval: mkEval defaultEval { handleAction = handleAction }
                  }
  
------------------------------------------------------------------------------- 
--- Main
-------------------------------------------------------------------------------

main :: Effect Unit
main =runHalogenAff do
  body <- awaitBody
  runUI app unit body
