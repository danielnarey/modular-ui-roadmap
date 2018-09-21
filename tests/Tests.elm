module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Dom
import Dom.Internal

import Html
import Html.Attributes as Attr


testString1 = "abc123"
testString2 = "xyz678"
testString3 = "quv999"
testString4 = "rst555"


suite : Test
suite =
  [ equalityTests
    |> describe "Testing equality of internal records"

  , modifierTests
    |> describe "Testing record updates in modifier functions"

  ]
    |> describe "Testing the `Dom` module"


equalityTests : List Test
equalityTests =
  [ ( \() ->
      Dom.element "div"
        |> Dom.getData
        |> Expect.equal div
    )
      |> test "`Dom.element` result should match the test record"

  , ( \() ->
      Dom.element "button"
        |> Dom.getData
        |> Expect.notEqual div
    )
      |> test "`Dom.element` result should not match the test record"

  , ( \() ->
      { div | classes = [ testString1, testString2 ] }
        |> Expect.notEqual { div | classes = [ testString2, testString1 ] }
    )
      |> test "Records should not be equal if listed items are in a different order"

  , ( \() ->
      Dom.element "div"
        |> Dom.render
        |> Expect.equal (Html.div [] [])
    )
      |> test "Rendered nodes should be equal if the tags are the same"

  , ( \() ->
      Dom.element "button"
        |> Dom.render
        |> Expect.notEqual (Html.div [] [])
    )
      |> test "Rendered nodes should not be equal if the tags are different"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass "container"
        |> Dom.render
        |> Expect.equal (Html.div [Attr.class "container"] [])
    )
      |> test "Rendered nodes should be equal if attribute values are the same"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass "column"
        |> Dom.render
        |> Expect.notEqual (Html.div [Attr.class "container"] [])
    )
      |> test "Rendered nodes should not be equal if attribute values are different"
  ]


modifierTests : List Test
modifierTests =
  [ ( \() ->
      Dom.element "div"
        |> Dom.setId testString1
        |> Dom.getData
        |> Expect.equal { div | id = testString1 }
    )
      |> test "`Dom.setId` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1 ] }
    )
      |> test "`Dom.addClass` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.addClassConditional testString2 True
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1, testString2 ] }
    )
      |> test "`Dom.addClassConditional` result should match a normal record update"

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
      |> test "`Dom.addClassList` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional [ testString1, testString2 ] True
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString1, testString2 ] }
    )
      |> test "`Dom.addClassListConditional` result should match a normal record update"

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
      |> test "`Dom.removeClass` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2 ]
        |> Dom.replaceClassList [ testString3, testString4 ]
        |> Dom.getData
        |> Expect.equal { div | classes = [ testString3, testString4 ] }
    )
      |> test "`Dom.replaceClassList` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2) ] }
    )
      |> test "`Dom.addstyle` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyle (testString1, testString2)
        |> Dom.addStyleConditional (testString3, testString4) True
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2), (testString3, testString4) ] }
    )
      |> test "`Dom.addStyleConditional` result should match a normal record update"

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
      |> test "`Dom.addStyleList` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleListConditional [ (testString1, testString2), (testString3, testString4) ] True
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString1, testString2), (testString3, testString4) ] }
    )
      |> test "`Dom.addStyleListConditional` result should match a normal record update"

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
      |> test "`Dom.removeStyle` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addStyleList [ (testString1, testString2) ]
        |> Dom.replaceStyleList [ (testString3, testString4) ]
        |> Dom.getData
        |> Expect.equal { div | styles = [ (testString3, testString4) ] }
    )
      |> test "`Dom.replaceStyleList` result should match a normal record update"

  ]



-- Helpers

{-| Construct an internal record for testing
-}
internal : String -> Dom.Internal.Data msg
internal tag =
  { tag = tag
  , id = ""
  , classes = []
  , styles = []
  , attributes = []
  , listeners = []
  , text = ""
  , children = []
  , namespace = ""
  , keys = []
  }


{-| Construct an empty div for testing
-}
div : Dom.Internal.Data msg
div =
  internal "div"
