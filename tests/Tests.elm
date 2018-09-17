module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import VirtualDom
import Dom


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
        |> Dom.getInternal
        |> Expect.equal div
    )
      |> test "`Dom.element` result should match the test record"

  , ( \() ->
      Dom.element "button"
        |> Dom.getInternal
        |> Expect.notEqual div
    )
      |> test "`Dom.element` result should not match the test record"

  , ( \() ->
      { div | classes = [ testString1, testString2 ] }
        |> Expect.notEqual { div | classes = [ testString2, testString1 ] }
    )
      |> test "Records should not match if listed items are in a different order"
  ]


modifierTests : List Test
modifierTests =
  [ ( \() ->
      Dom.element "div"
        |> Dom.setId testString1
        |> Dom.getInternal
        |> Expect.equal { div | id = testString1 }
    )
      |> test "`Dom.setId` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClass testString1
        |> Dom.getInternal
        |> Expect.equal { div | classes = [ testString1 ] }
    )
      |> test "`Dom.addClass` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassConditional (testString1, True)
        |> Dom.getInternal
        |> Expect.equal { div | classes = [ testString1 ] }
    )
      |> test "`Dom.addClassConditional` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassConditional (testString1, False)
        |> Dom.getInternal
        |> Expect.equal div
    )
      |> test "`Dom.addClassConditional` result should match the unmodified test record"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2 ]
        |> Dom.getInternal
        |> Expect.equal { div | classes = [ testString1, testString2 ] }
    )
      |> test "`Dom.addClassList` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional ([ testString1, testString2 ], True)
        |> Dom.getInternal
        |> Expect.equal { div | classes = [ testString1, testString2 ] }
    )
      |> test "`Dom.addClassListConditional` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassListConditional ([ testString1, testString2 ], False)
        |> Dom.getInternal
        |> Expect.equal div
    )
      |> test "`Dom.addClassListConditional` result should match the unmodified test record"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2, testString1 ]
        |> Dom.removeClass testString1
        |> Dom.getInternal
        |> Expect.equal { div | classes = [ testString2 ] }
    )
      |> test "`Dom.removeClass` result should match a normal record update"

  , ( \() ->
      Dom.element "div"
        |> Dom.addClassList [ testString1, testString2 ]
        |> Dom.replaceClassList [ testString3, testString4 ]
        |> Dom.getInternal
        |> Expect.equal { div | classes = [ testString3, testString4 ] }
    )
      |> test "`Dom.replaceClassList` result should match a normal record update"
  ]



-- Helpers

{-| Copy of `Internal` type for testing
-}
type alias Internal msg =
  { tag : String
  , id : String
  , classes : List String
  , styles : List (String, String)
  , actions: List (String, msg)
  , attributes : List (VirtualDom.Attribute msg)
  , text : String
  , children : List (VirtualDom.Node msg)
  , namespace : String
  , keys : List String
  }


{-| Construct an internal record for testing
-}
internal : String -> Internal msg
internal tag =
  { tag = tag
  , id = ""
  , classes = []
  , styles = []
  , actions = []
  , attributes = []
  , text = ""
  , children = []
  , namespace = ""
  , keys = []
  }


{-| Construct an empty div for testing
-}
div : Internal msg
div =
  internal "div"
