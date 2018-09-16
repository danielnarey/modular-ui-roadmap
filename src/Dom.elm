module Dom exposing
  ( Element )


import VirtualDom


{-| A record containing all of the data needed to construct an HTML node (via
`VirtualDom.Node`). By using a record to temporarily store data about a node,
we can partially construct that node with some data, but delay building it until
all of the data has been assembled. In this way, all of a node's data is
available to be modified until it is either placed in a container element or
passed as an argument to the `render` function.

-}
type alias Element msg =
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


{-| Constructor for `Element` records. The string argument provides the HTML
tag.

-}
element : String -> Element msg
element tag =
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


-- Modifier functions for attributes:
---- A "modifier" is any function that takes an existing element record,
---- updates some of its internal data, and returns the updated element record.


-- ID

{-| Set the id attribute on an `Element` record
-}
setId : String -> Element msg -> Element msg
setId idString current =
  { current | id = idString }


-- CLASS

{-| Add a class name to the current list contained in an `Element` record

-}
addClass : String -> Element msg -> Element msg
addClass newClass current =
  { current | classes = [ newClass ] |> List.append current.classes }


{-| Add a class name to the current list contained in an `Element` record when
the boolean argument evaluates to `True`

-}
addClassConditional : (String, Bool) -> Element msg -> Element msg
addClassConditional (newClass, test) =
  case test of
    True ->
      addClass newClass

    False ->
      identity


{-| Add a list of class names to the current list contained in an `Element`
record

-}
addClassList : List String -> Element msg -> Element msg
addClassList newClassList current =
  { current | classes = newClassList |> List.append current.classes }


{-| Add a list of class names to the current list contained in an `Element`
record when the boolean argument evaluates to `True`

-}
addClassListConditional : (List String, Bool) -> Element msg -> Element msg
addClassListConditional (newClassList, test) =
  case test of
    True ->
      addClassList newClassList

    False ->
      identity


{-| Delete all instances of a class name or from the current list contained in
an `Element` record

-}
removeClass : String -> Element msg -> Element msg
removeClass classToRemove current =
  { current | classes = current.classes |> List.filter ((/=) classToRemove) }


{-| Delete the current list of class names contained in current `Element`
record, replacing it a new list of class names

-}
replaceClassList : List String -> Element msg -> Element msg
replaceClassList newClassList current =
  { current | classes = newClassList }


-- STYLE

{-| Add a style key/value pair to the current list contained in an `Element`
record

-}
addStyle : (String, String) -> Element msg -> Element msg
addStyle newStyle current =
  { current | styles = [ newStyle ] |> List.append current.styles }


{-| Add a style key/value pair to the current list contained in an `Element`
record when the boolean argument evaluates to `True`

-}
addStyleConditional : ((String, String), Bool) -> Element msg -> Element msg
addStyleConditional (newStyle, test) =
  case test of
    True ->
      addStyle newStyle

    False ->
      identity

-- THIS IS WHERE I LEFT OFF...

addStyleList : List (String, String) -> Element msg -> Element msg
addStyleListConditional : (List (String, String), Bool) -> Element msg -> Element msg
removeStyle : String -> Element msg -> Element msg
replaceStyleList : List (String, String) -> Element msg -> Element msg

-- event listeners that don't capture an input value
addAction : (String, msg) -> Element msg -> Element msg
addActionConditional : ((String, msg), Bool) -> Element msg -> Element msg
addActionStopPropagation : (String, msg) -> Element msg -> Element msg
addActionPreventDefault : (String, msg) -> Element msg -> Element msg
addActionStopAndPrevent : (String, msg) -> Element msg -> Element msg
removeAction : String -> Element msg -> Element msg

-- event listeners that capture an input value (immediately rendered to `VirtualDom.Attribute`)
setInputHandler : (String -> msg) -> Element msg -> Element msg
setInputHandlerWithParser : (a -> msg, String -> a) -> Element msg -> Element msg
setCheckHandler : (Bool -> msg) -> Element msg -> Element msg
setCustomHandler : (String, Decoder msg) -> Element msg -> Element msg
setCustomHandlerStopPropagation : (String, Decoder msg) -> Element msg -> Element msg
setCustomHandlerPreventDefault : (String, Decoder msg) -> Element msg -> Element msg
setCustomHandlerStopAndPrevent : (String, Decoder msg) -> Element msg -> Element msg

-- add any other attribute using an `Html.Attribute` function or `VirtualDom` primitive
addAttribute : VirtualDom.Attribute msg -> Element msg -> Element msg
addAttributeConditional : (VirtualDom.Attribute msg, Bool) -> Element msg -> Element msg
addAttributeList : List (VirtualDom.Attribute msg) -> Element msg -> Element msg
addAttributeListConditional : (List (VirtualDom.Attribute msg), Bool) -> Element msg -> Element msg
replaceAttributeList : List (VirtualDom.Attribute msg) -> Element msg -> Element msg

-- Modifier functions for internal text:
-- Text is rendered to VirtualDom as the first child node, containing plain HTML text. Helper functions for styling text that were included in the previous version will be moved to a separate package.

appendText : String -> Element msg -> Element msg
appendTextConditional : (String, Bool) -> Element msg -> Element msg
prependText : String -> Element msg -> Element msg
prependTextConditional : (String, Bool) -> Element msg -> Element msg
replaceText : String -> Element msg -> Element msg
replaceTextConditional : (String, Bool) -> Element msg -> Element msg

-- Modifier functions to construct an element's internal tree:
-- Child elements are immediately rendered to VirtualDom when this function is executed. setChildListWithKeys uses the VirtualDom.KeyedNode optimization and should only be called once on any given node.

appendChild : Element msg -> Element msg -> Element msg
appendChildConditional : (Element msg, Bool) -> Element msg -> Element msg
appendChildList : List (Element msg) -> Element msg -> Element msg
appendChildListConditional : (List (Element msg), Bool) -> Element msg -> Element msg
appendNodeList : List (VirtualDom.Node msg) -> Element msg -> Element msg
prependChild : Element msg -> Element msg -> Element msg
prependChildConditional : (Element msg, Bool) -> Element msg -> Element msg
prependChildList : List (Element msg) -> Element msg -> Element msg
prependChildListConditional : (List (Element msg), Bool) -> Element msg -> Element msg
prependNodeList : List (VirtualDom.Node msg) -> Element msg -> Element msg
replaceChildList : List (Element msg) -> Element msg -> Element msg
replaceNodeList : List (VirtualDom.Node msg) -> Element msg -> Element msg
setChildListWithKeys : List (String, Element msg) -> Element msg -> Element msg
setNodeListWithKeys : List (String, VirtualDom.Node msg) -> Element msg -> Element msg

-- Modifier functions to set tag and namespace
-- The tag is already set with the element constructor function, but it could be useful to be able to change it when using component libraries. Namespace would typically be used to construct SVG nodes. Whenever the namespace field is not an empty string, VirtualDom.nodeNS is used for rendering.

setTag : String -> Element msg -> Element msg
setNamespace : String -> Element msg -> Element msg

-- Rendering
-- This function only needs to be called on the root node of a tree. VirtualDom.Node is interchangeable with Html.Html.

render : Element msg -> VirtualDom.Node msg
render root =
  let
    consId attributeList =
      case root.id of
        "" ->
          attributeList

        _ ->
          ( root.id
            |> Json.Encode.string
            |> VirtualDom.property "id"
          )
            :: attributeList

    consClassName attributeList =
      case root.classes of
        [] ->
          attributeList

        _ ->
          ( root.classes
            |> String.join " "
            |> String.trim
            |> Json.Encode.string
            |> VirtualDom.property "className"
          )
            :: attributeList

    consNamespace attributeList =
      case root.namespace of
        "" ->
          attributeList

        _ ->
          ( root.namespace
            |> Json.Encode.string
            |> VirtualDom.property "namespace"
          )
            :: attributeList

    consText childList =
      case root.text of
          "" ->
            childList

          _ ->
            ( root.text
              |> VirtualDom.text
            )
              :: childList

    consTextKeyed keyedList =
      case root.text of
          "" ->
            keyedList

          _ ->
            ( root.text
              |> VirtualDom.text
              |> (,) ("internal-text")
            )
              :: keyedList

  in
    case root.keys of
      [] ->
        root.children
          |> consText
          |> VirtualDom.node root.tag
            ( root.attributes
              |> consId
              |> consClassName
              |> consNamespace
            )

      _ ->
        root.children
          |> List.map2 (,) root.keys
          |> consTextKeyed
          |> VirtualDom.keyedNode root.tag
            ( root.attributes
              |> consId
              |> consClassName
              |> consNamespace
            )
