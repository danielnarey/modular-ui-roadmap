module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Dom
import Dom.Internal

import Html
import Html.Attributes as Attr


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
      { div | children = [ Html.p [] [] ] }
        |> Expect.equal { div | children = [ Html.p [] [] ] }
    )
      |> test "Records containing child nodes with the same tag should be equal"

  , ( \() ->
      { div | children = [ Html.button [] [] ] }
        |> Expect.notEqual { div | children = [ Html.p [] [] ] }
    )
      |> test "Records containing child nodes with different tags should not be equal"

  , ( \() ->
      { div | children = [ Html.p [] [ Html.span [] [ Html.text "something" ] ] ] }
        |> Expect.equal { div | children = [ Html.p [] [ Html.span [] [ Html.text "something" ] ] ] }
    )
      |> test "Records containing equivalent child nodes with identical descendant trees should be equal"

  , ( \() ->
      { div | children = [ Html.p [] [ Html.span [] [ Html.text "something" ] ] ] }
        |> Expect.notEqual { div | children = [ Html.p [] [ Html.span [] [ Html.text "something else" ] ] ] }
    )
      |> test "Records containing equivalent child nodes with diverging descendant trees should not be equal"

  ]


id : List Test
id =
  [ ( \() ->
      Dom.element "div"
        |> Dom.setId testString1
        |> Dom.getData
        |> Expect.equal { div | id = testString1 }
    )
      |> test "`Dom.setId` result should match a test record update"

  ]


classes : List Test
classes =
  [ ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1 ] }
    )
      |> test "`Dom.addClass` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassConditional testString2 True
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1, testString2 ] }
    )
      |> test "`Dom.addClassConditional` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassConditional testString2 False
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1 ] }
    )
      |> test "`Dom.addClassConditional` result should match the unmodified test record"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassList [ testString2, testString3 ]
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1, testString2, testString3 ] }
    )
      |> test "`Dom.addClassList` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional [ testString1, testString2 ] True
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1, testString2 ] }
    )
      |> test "`Dom.addClassListConditional` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional [ testString1, testString2 ] False
        |> Dom.getData
        |> Expect.equal div
    )
      |> test "`Dom.addClassListConditional` result should match the unmodified test record"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2, testString1 ]
        |> Dom.removeClass testString1
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString2 ] }
    )
      |> test "`Dom.removeClass` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2 ]
        |> Dom.replaceClassList [ testString3, testString4 ]
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString3, testString4 ] }
    )
      |> test "`Dom.replaceClassList` result should match a test record update"

  ]


styles : List Test
styles =
  [ ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2) ] }
    )
      |> test "`Dom.addstyle` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.addStyleConditional (testString3, testString4) True
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2), (testString3, testString4) ] }
    )
      |> test "`Dom.addStyleConditional` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.addStyleConditional (testString3, testString4) False
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2) ] }
    )
      |> test "`Dom.addStyleConditional` result should match the unmodified test record"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2), (testString3, testString4) ]
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2), (testString3, testString4) ] }
    )
      |> test "`Dom.addStyleList` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleListConditional [ (testString1, testString2), (testString3, testString4) ] True
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2), (testString3, testString4) ] }
    )
      |> test "`Dom.addStyleListConditional` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleListConditional [ (testString1, testString2), (testString3, testString4) ] False
        |> Dom.getData
        |> Expect.equal div
    )
      |> test "`Dom.addStyleListConditional` result should match the unmodified test record"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2), (testString3, testString4), (testString1, testString4) ]
        |> Dom.removeStyle testString1
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString3, testString4) ] }
    )
      |> test "`Dom.removeStyle` result should match a test record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2) ]
        |> Dom.replaceStyleList [ (testString3, testString4) ]
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString3, testString4) ] }
    )
      |> test "`Dom.replaceStyleList` result should match a test record update"

  ]
