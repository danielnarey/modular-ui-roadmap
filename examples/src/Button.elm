module Button exposing (main)


import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Dom


main : Program () Int Click
main =
  { init = 0
  , view = view
  , update = \click count -> count + 1
  }
    |> Browser.sandbox


type Click = Click


view : Int -> Html Click
view current =
  let
    heading =
      Dom.element "h1"
        |> Dom.addClass "pb-3"
        |> Dom.appendText "Button.elm"

  in
    Dom.element "div"
      |> Dom.addClassList
        [ "mx-auto"
        , "my-4"
        , "p-4"
        , "border"
        , "rounded"
        ]
      |> Dom.addStyle ("maxWidth", "500px")
      |> Dom.appendChildList
        [ heading
        , example current
        ]
      |> Dom.render


-- main interactive component

example : Int -> Dom.Element Click
example current =
  let
    counterText =
      [ "Button has been clicked "
      , current |> String.fromInt
      , case current of
          1 -> "time"
          _ -> "times"
      ]
        |> String.join " "

    counter =
      ( case current of
        0 -> "secondary"
        _ -> "primary"
      )
        |> alert
        |> Dom.appendText counterText

    clicker =
      button "primary"
        |> Dom.addAction ("click", Click)
        |> Dom.appendText "Click Me!"

  in
    Dom.element "div"
      |> Dom.addClassList
        [ "p-5"
        , "text-center"
        , "bg-light"
        , "rounded"
        ]
      |> Dom.appendChildList
        [ counter
        , Dom.element "div" |> Dom.appendChild clicker
        ]


-- simple reusable components

alert : String -> Dom.Element msg
alert context =
  Dom.element "div"
    |> Dom.addClassList
      [ "alert"
      , "alert-" ++ context
      ]
    |> Dom.addAttribute (Attr.attribute "role" "alert")


button : String -> Dom.Element msg
button context =
  Dom.element "button"
    |> Dom.addClassList
      [ "btn"
      , "btn-" ++ context
      ]
    |> Dom.addAttribute
      ( Attr.type_ "button" )