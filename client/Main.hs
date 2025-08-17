{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso

type Model = ()

data Action = None

updateModel :: Action -> Transition Model Action
updateModel _ = io_ $ consoleLog "Unreachable."

viewModel :: Model -> View Model Action
viewModel () = p_ [] [ text "Hello, client!" ]

app :: App Model Action
app = component () updateModel viewModel

main :: IO () 
main = run $ startApp app
