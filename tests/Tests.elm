module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Dom
import Dom.Internal

import VirtualDom
import Html
import Html.Attributes as Attr
import Html.Events as Event


-- HELPERS --

{-| Construct an empty div for testing
-}
div : Dom.Internal.Data msg
div =
  { tag = "div"
  , id = ""
  , classes = []
  , styles = []
  , listeners = []
  , attributes = []
  , text = ""
  , children = []
  , namespace = ""
  , keys = []
  }


testString1 = "abc123"
testString2 = "xyz678"
testString3 = "quv999"
testString4 = "rst555"

type TestMsg
  = DoSomething
  | CaptureInput String

-- TESTS --


suite : Test
suite =
  [ recordEquality
    |> describe "Do comparisons of Elm records give the expected results?"

  , nodeEquality
    |> describe "Do comparisons of VirtualDom nodes give the expected results?"

  , [ id
      |> describe "Do updates to the `id` field give the expected results?"

    , classes
      |> describe "Do updates to the `classes` field give the expected results?"

    , styles
      |> describe "Do updates to the `styles` field give the expected results?"

    , listeners
      |> describe "Do updates to the `listeners` field give the expected results?"

    ]
      |> describe "Do `Element` record update functions give the expected results?"

  ]
    |> describe "Testing `Dom.elm`"


recordEquality : List Test
recordEquality =
  [ ( \() ->
      Dom.element "div"
        |> Dom.getData
        |> Expect.equal div
    )
      |> test "Element records with the same tag and no other data should be equal"

  , ( \() ->
      Dom.element "button"
        |> Dom.getData
        |> Expect.notEqual div
    )
      |> test "Element records with different tags should not be equal"

  , ( \() ->
      { div | classes = [ testString1, testString2 ] }
        |> Expect.notEqual { div | classes = [ testString2, testString1 ] }
    )
      |> test "Records should not be equal if listed items are in a different order"

  ]


nodeEquality : List Test
nodeEquality =
  [ ( \() ->
      Dom.element "div"
        |> Dom.render
        |> Expect.equal (Html.div [] [])
    )
      |> test "Rendered nodes with the same tag and no other data should be equal"

  , ( \() ->
      Dom.element "button"
        |> Dom.render
        |> Expect.notEqual (Html.div [] [])
    )
      |> test "Rendered nodes with different tags should not be equal"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass "container"
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class "container"] [])
    )
      |> test "Rendered nodes with the same attribute value and no other data should be equal"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass "column"
        |> Dom.render
        |> Expect.notEqual (Html.div [Attr.class "container"] [])
    )
      |> test "Rendered nodes with different values for an attribute should not be equal"

  , ( \() ->
      Dom.element "div"
        |> Dom.appendChild (Dom.element "p")
        |> Dom.render
        |> Expect.equal (Html.div [] [ Html.p [] [] ])
    )
      |> test "Rendered nodes containing child nodes with the same tag and no other data should be equal"

  , ( \() ->
      Dom.element "div"
        |> Dom.appendChild (Dom.element "button")
        |> Dom.render
        |> Expect.notEqual (Html.div [] [ Html.p [] [] ])
    )
      |> test "Rendered nodes containing child nodes with different tags should not be equal"

  , ( \() ->
      Dom.element "div"
        |> Dom.appendChild
          ( Dom.element "p"
            |> Dom.appendChild (Dom.element "span" |> Dom.appendText "something")
          )
        |> Dom.render
        |> Expect.equal (Html.div [] [ Html.p [] [ Html.span [] [ Html.text "something" ] ] ])
    )
      |> test "Records containing equivalent child nodes with identical descendant trees should be equal"

  , ( \() ->
      Dom.element "div"
        |> Dom.appendChild
          ( Dom.element "p"
            |> Dom.appendChild (Dom.element "span" |> Dom.appendText "something else")
          )
        |> Dom.render
        |> Expect.notEqual (Html.div [] [ Html.p [] [ Html.span [] [ Html.text "something" ] ] ])
    )
      |> test "Records containing equivalent child nodes with diverging descendant trees should not be equal"

  ]


id : List Test
id =
  [ ( \() ->
      Dom.element "div"
        |> Dom.setId testString1
        |> Dom.render
        |> Expect.equal (Html.div [Attr.id testString1] [])
    )
      |> test "Dom.setId"

  ]


classes : List Test
classes =
  [ ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString1] [])
    )
      |> test "Dom.addClass"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassConditional testString2 True
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString1, Attr.class testString2] [])
    )
      |> test "Dom.addClassConditional: condition is True"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassConditional testString2 False
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString1] [])
    )
      |> test "Dom.addClassConditional: condition is False"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassList [ testString2, testString3 ]
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString1, Attr.class testString2, Attr.class testString3] [])
    )
      |> test "Dom.addClassList"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional [ testString1, testString2 ] True
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString1, Attr.class testString2] [])
    )
      |> test "Dom.addClassListConditional: condtion is True"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional [ testString1, testString2 ] False
        |> Dom.render
        |> Expect.equal (Html.div [] [])
    )
      |> test "Dom.addClassListConditional: condtion is False"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2, testString1 ]
        |> Dom.removeClass testString1
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString2] [])
    )
      |> test "Dom.removeClass"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2 ]
        |> Dom.replaceClassList [ testString3, testString4 ]
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class testString3, Attr.class testString4] [])
    )
      |> test "Dom.replaceClassList"

  ]


styles : List Test
styles =
  [ ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString1 testString2] [])
    )
      |> test "Dom.addstyle"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.addStyleConditional (testString3, testString4) True
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString1 testString2, Attr.style testString3 testString4] [])
    )
      |> test "Dom.addStyleConditional: condition is True"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.addStyleConditional (testString3, testString4) False
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString1 testString2] [])
    )
      |> test "Dom.addStyleConditional: condition is False"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2), (testString3, testString4) ]
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString1 testString2, Attr.style testString3 testString4] [])
    )
      |> test "Dom.addStyleList"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleListConditional [ (testString1, testString2), (testString3, testString4) ] True
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString1 testString2, Attr.style testString3 testString4] [])
    )
      |> test "Dom.addStyleListConditional: condition is True"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleListConditional [ (testString1, testString2), (testString3, testString4) ] False
        |> Dom.render
        |> Expect.equal (Html.div [] [])
    )
      |> test "Dom.addStyleListConditional: condition is False"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2), (testString3, testString4), (testString1, testString4) ]
        |> Dom.removeStyle testString1
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString3 testString4] [])
    )
      |> test "Dom.removeStyle"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2) ]
        |> Dom.replaceStyleList [ (testString3, testString4) ]
        |> Dom.render
        |> Expect.equal (Html.div [Attr.style testString3 testString4] [])
    )
      |> test "Dom.replaceStyleList"

  ]


listeners : List Test
listeners =
  [ ( \() ->
      Dom.element "div"
        |> Dom.addAction ("click", DoSomething)
        |> Dom.render
        |> Expect.equal (Html.div [Event.onClick DoSomething] [])
    )
      |> test "Dom.addAction"

  , ( \() ->
      Dom.element "div"
        |> Dom.addActionConditional ("click", DoSomething) True
        |> Dom.render
        |> Expect.equal (Html.div [Event.onClick DoSomething] [])
    )
      |> test "Dom.addActionConditional: condition is True"

  , ( \() ->
      Dom.element "div"
        |> Dom.addActionConditional ("click", DoSomething) False
        |> Dom.render
        |> Expect.equal (Html.div [] [])
    )
      |> test "Dom.addActionConditional: condition is False"

  , ( \() ->
      Dom.element "div"
        |> Dom.addInputHandler CaptureInput
        |> Dom.getData >> .listeners
        |> List.map (\(k, v) -> VirtualDom.on k v)
        |> Debug.toString
        |> Expect.equal ([Event.onInput CaptureInput] |> Debug.toString)
    )
      |> test "Dom.addInputHandler"

  ]
