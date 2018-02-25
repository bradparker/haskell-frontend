{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Monoid           ((<>))
import           GHCJSDevServer.Client (runGHCJSDevServerClient)
import           Miso                  (App (..), Attribute, Effect, View,
                                        button_, defaultEvents, div_, form_,
                                        h1_, input_, li_, noEff, onInput,
                                        onSubmit, startApp, type_, ul_, value_)
import           Miso.String           (MisoString, toMisoString)
import           MisoStyle             (StyledView, Styles, fontFamily, margin,
                                        padding, property, styled, styledView,
                                        styles, text, unstyled)

data Status
  = Pending
  | Complete
  deriving (Eq)

data Todo = Todo
  { _id    :: Int
  , value  :: MisoString
  , status :: Status
  } deriving (Eq)

data Model = Model
  { newTodo :: Todo
  , todos   :: [Todo]
  } deriving (Eq)

data Message
  = NoOp
  | UpdateNew MisoString
  | SubmitNew Todo
  | ChangeStatus Status
  | Remove Int

appStyles :: Styles
appStyles = styles $ do fontFamily "sans-serif"

container :: [Attribute Message] -> [StyledView Message] -> StyledView Message
container =
  styled div_ $
  styles $ do
    property "max-width" "24rem"
    margin "0 auto"

header :: StyledView Message
header = unstyled h1_ [] [text "These are the things you must do"]

newTodoForm :: Todo -> StyledView Message
newTodoForm todo =
  unstyled
    form_
    [onSubmit (SubmitNew todo)]
    [ unstyled input_ [type_ "text", value_ (value todo), onInput UpdateNew] []
    , unstyled button_ [type_ "submit"] [text "Create"]
    ]

todoView :: Todo -> StyledView Message
todoView todo = unstyled div_ [] [text (value todo)]

todoList :: [Todo] -> StyledView Message
todoList todos =
  unstyled
    ul_
    []
    (map (styled li_ (styles (padding "1rem")) [] . pure . todoView) todos)

app :: Model -> StyledView Message
app model =
  styled
    div_
    appStyles
    []
    [container [] [header, newTodoForm (newTodo model), todoList (todos model)]]

initialModel :: Model
initialModel =
  Model {newTodo = Todo {_id = 1, value = "", status = Pending}, todos = []}

updateModel :: Message -> Model -> Effect Message Model
updateModel (UpdateNew value) model =
  noEff (model {newTodo = (newTodo model) {value = value}})
updateModel (SubmitNew todo) model =
  noEff
    (model
     { newTodo = Todo {_id = _id todo + 1, value = "", status = Pending}
     , todos = todo : todos model
     })
updateModel NoOp model = noEff model

updateView :: Model -> View Message
updateView = styledView app

main :: IO ()
main = do
  runGHCJSDevServerClient
  startApp
    App
    { initialAction = NoOp
    , model = initialModel
    , update = updateModel
    , view = updateView
    , events = defaultEvents
    , subs = []
    , mountPoint = Nothing
    }
