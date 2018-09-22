module Hover exposing (main)


import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Dom


main : Program () State Toggle
main =
  { init = { listening = False, hovering = False }
  , view = view
  , update = update
  }
    |> Browser.sandbox


-- model type alias

type alias State =
  { listening : Bool
  , hovering : Bool
  }


-- message type

type Toggle
  = Listen
  | Hover


-- UPDATE --

update : Toggle -> State -> State
update msg state =
  case msg of
    Listen -> { state | listening = not state.listening }
    Hover -> { state | hovering = not state.hovering }


-- VIEW --

view : State -> Html Toggle
view state =
  let
    heading =
      Dom.element "h1"
        |> Dom.addClass "pb-3"
        |> Dom.appendText "Hover.elm"

  in
    Dom.element "div"
      |> Dom.addClassList
        [ "mx-auto"
        , "my-4"
        , "p-4"
        , "border"
        , "rounded"
        ]
      |> Dom.addStyleList
        [ ("maxWidth", "500px")
        , ("height", "450px")
        ]
      |> Dom.appendChildList
        [ heading
        , example state
        ]
      |> Dom.render


-- main interactive component

example : State -> Dom.Element Toggle
example state =
  let
    hoverCircle =
      circle 150
        |> Dom.addClassList
          [ "mx-auto"
          , "mb-3"
          , "bg-warning"
          ]
        |> Dom.addClassConditional "bg-danger" state.hovering
        |> case state.listening of
          True -> hoverable Hover
          False -> identity

    controller =
      ( case state.listening of
        True -> "success"
        False -> "secondary"
      )
        |> button
        |> Dom.addAction ("click", Listen)
        |> Dom.appendText
          ( case state.listening of
            True -> "Listening On"
            False -> "Listening Off"
          )

  in
    Dom.element "div"
      |> Dom.addClassList
        [ "p-5"
        , "text-center"
        , "bg-light"
        , "rounded"
        ]
      |> Dom.appendChildList
        [ hoverCircle
        , Dom.element "div" |> Dom.appendChild controller
        ]


-- simple reusable components

button : String -> Dom.Element msg
button context =
  Dom.element "button"
    |> Dom.addClassList
      [ "btn"
      , "btn-" ++ context
      ]
    |> Dom.addAttribute
      ( Attr.type_ "button" )


circle : Int -> Dom.Element msg
circle diameter =
  let
    diameterPx =
      (diameter |> String.fromInt) ++ "px"

  in
    Dom.element "div"
      |> Dom.addClass "rounded-circle"
      |> Dom.addStyleList
        [ ("height", diameterPx)
        , ("width", diameterPx)
        ]


hoverable : msg -> Dom.Element msg -> Dom.Element msg
hoverable toggle =
  Dom.addAction ("mouseover", toggle)
    >> Dom.addAction ("mouseout", toggle)
