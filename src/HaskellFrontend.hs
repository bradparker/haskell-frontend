{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HaskellFrontend
  ( main
  ) where

import qualified Miso
import Miso
  ( App(App)
  , Effect
  , View
  , button_
  , defaultEvents
  , div_
  , noEff
  , onClick
  , startApp
  , text
  )
import Miso.String (ms)

type Model = Int

data Action
  = AddOne
  | SubtractOne
  | NoOp
  deriving (Show, Eq)

main :: IO ()
main =
  startApp App
    { Miso.initialAction = NoOp
    , Miso.model         = 0
    , Miso.update        = update
    , Miso.view          = view
    , Miso.events        = defaultEvents
    , Miso.subs          = []
    , Miso.mountPoint    = Nothing
    }

update :: Action -> Model -> Effect Action Model
update AddOne m = noEff (m + 1)
update SubtractOne m = noEff (m - 1)
update NoOp m = noEff m

view :: Model -> View Action
view x = div_
  []
  [ button_ [onClick AddOne] [text "+"]
  , text (ms x)
  , button_ [onClick SubtractOne] [text "-"]
  ]
