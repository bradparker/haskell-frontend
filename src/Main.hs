{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Bool             (bool)
import           Data.Monoid           ((<>))
import           GHCJSDevServer.Client (runGHCJSDevServerClient)
import           Miso                  (App (..), Attribute, Checked (Checked),
                                        Effect, View, button_, checked_,
                                        defaultEvents, div_, for_, form_, h1_,
                                        id_, input_, label_, li_, noEff,
                                        onChecked, onClick, onInput, onSubmit,
                                        startApp, type_, ul_, value_)
import           Miso.String           (MisoString, toMisoString)
import           MisoStyle             (StyleBuilder, StyledView, Styles,
                                        backgroundColor, border, borderRadius,
                                        borderTop, color, cursor, display,
                                        fontFamily, fontSize, fontWeight, hover,
                                        lineHeight, margin, marginBottom,
                                        marginRight, padding, paddingLeft,
                                        property, pseudo, styled, styledView,
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
  | ChangeStatus Int
                 Status
  | Remove Int

appStyles :: Styles
appStyles = styles $ fontFamily "sans-serif"

container :: [Attribute Message] -> [StyledView Message] -> StyledView Message
container =
  styled div_ $
  styles $ do
    property "max-width" "24rem"
    margin "0 auto"
    padding "2rem"

header ::
     ([Attribute Message] -> [View Message] -> View Message)
  -> StyleBuilder Styles
  -> [Attribute Message]
  -> [StyledView Message]
  -> StyledView Message
header element extraStyles =
  styled element $
  styles $ do
    lineHeight "1.125em"
    fontWeight "700"
    extraStyles

heading :: StyledView Message
heading =
  header h1_ (margin "0 0 2rem") [] [text "These are the things you must do"]

globalStyles :: StyleBuilder Styles
globalStyles = do
  property "box-sizing" "border-box"
  property "appearance" "none"
  fontFamily "inherit"
  fontSize "inherit"

pillStyles :: StyleBuilder Styles
pillStyles = do
  borderRadius "0.25rem"
  padding "0.75em 1em"
  lineHeight "1rem"

textInput :: MisoString -> (MisoString -> Message) -> StyledView Message
textInput val handler =
  styled input_ textInputStyles [type_ "text", value_ val, onInput handler] []
  where
    textInputStyles =
      styles $ do
        globalStyles
        pillStyles
        property "flex" "1 1 100%"
        border "thin solid lightgrey"

submitButton :: [StyledView Message] -> StyledView Message
submitButton = styled button_ buttonStyles [type_ "submit"]
  where
    buttonStyles =
      styles $ do
        globalStyles
        pillStyles
        property "flex" "1 1 auto"
        border "none"
        color "white"
        backgroundColor "seagreen"
        margin "0 0 0 1rem"
        cursor "pointer"
        hover $ backgroundColor "darkgreen"

newTodoForm :: Todo -> StyledView Message
newTodoForm todo =
  styled
    form_
    formStyles
    [onSubmit (SubmitNew todo)]
    [textInput (value todo) UpdateNew, submitButton [text "Create"]]
  where
    formStyles =
      styles $ do
        display "flex"
        marginBottom "1.5rem"

todoView :: Todo -> StyledView Message
todoView todo =
  styled
    div_
    todoStyles
    []
    [ styled
        div_
        checkboxStyle
        []
        [ unstyled
            input_
            [ type_ "checkbox"
            , onChecked checkboxHandler
            , checked_ (status todo == Complete)
            , id_ ("todo-" <> toMisoString (_id todo))
            ]
            []
        ]
    , styled
        label_
        nameStyle
        [for_ ("todo-" <> toMisoString (_id todo))]
        [text (value todo)]
    , styled button_ buttonStyle [onClick (Remove (_id todo))] [text "Remove"]
    ]
  where
    checkboxHandler (Checked checked) =
      bool
        (ChangeStatus (_id todo) Pending)
        (ChangeStatus (_id todo) Complete)
        checked
    checkboxStyle =
      styles $ do
        cursor "pointer"
        fontSize "inherit"
    nameStyle =
      styles $ do
        paddingLeft "1rem"
        cursor "pointer"
        property "flex" "1 1 100%"
        if status todo == Complete
          then property "text-decoration" "line-through"
          else property "text-decoration" "none"
    buttonStyle =
      styles $ do
        pillStyles
        cursor "pointer"
        fontSize "inherit"
        backgroundColor "whitesmoke"
        border "thin solid lightgrey"
        hover $ backgroundColor "lightgrey"
    todoStyles =
      styles $ do
        display "flex"
        property "align-items" "center"
        lineHeight "1.25rem"

todoList :: [Todo] -> StyledView Message
todoList todos =
  styled
    ul_
    todoListStyles
    []
    (map (styled li_ todoListItemStyles [] . pure . todoView) todos)
  where
    todoListItemStyles =
      styles $ do
        globalStyles
        margin "0"
        padding "0.5rem 0"
        borderTop "thin solid lightgrey"
    todoListStyles =
      styles $ do
        globalStyles
        margin "0"
        padding "0"
        property "list-style" "none"

app :: Model -> StyledView Message
app model =
  styled
    div_
    appStyles
    []
    [ container
        []
        [heading, newTodoForm (newTodo model), todoList (todos model)]
    ]

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
updateModel (ChangeStatus targetId newStatus) model =
  noEff
    (model
     { todos =
         map
           (\todo ->
              if _id todo == targetId
                then todo {status = newStatus}
                else todo)
           (todos model)
     })
updateModel (Remove targetId) model =
  noEff (model {todos = filter ((targetId /=) . _id) (todos model)})
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
