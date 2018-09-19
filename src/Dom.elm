module Dom exposing
  ( Element, element
  , setId
  , addClass, addClassConditional
  , addClassList, addClassListConditional
  , removeClass, replaceClassList
  , addStyle, addStyleConditional
  , addStyleList, addStyleListConditional
  , removeStyle, replaceStyleList
  , getInternal
  )


import VirtualDom
import Json.Decode

import Dom.Internal as Internal


{-| A record containing all of the data needed to construct an HTML node (via
`VirtualDom.Node`). By using a record to temporarily store data about a node,
we can partially construct that node with some data, but delay building it until
all of the data has been assembled. In this way, all of a node's data is
available to be modified until it is either placed in a container element or
passed as an argument to the `render` function.

-}
type alias Element msg =
  Internal.Element msg


-- CONSTRUCTOR

{-| Constructor for `Element` records. The string argument provides the HTML
tag.

-}
element : String -> Element msg
element tag =
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
    |> Internal.Element


-- Modifier functions for attributes:
---- A "modifier" is any function that takes an existing element record,
---- updates some of its internal data, and returns the updated element record.


-- ID

{-| Set the id attribute on an `Element` record
-}
setId : String -> Element msg -> Element msg
setId s =
  Internal.modify (\n -> { n | id = s })


-- CLASS

{-| Add a class name to the current list contained in an `Element` record

-}
addClass : String -> Element msg -> Element msg
addClass s =
  Internal.modify (\n -> { n | classes = List.append n.classes [s] })


{-| Add a class name to the current list contained in an `Element` record when
the boolean argument evaluates to `True`

-}
addClassConditional : (String, Bool) -> Element msg -> Element msg
addClassConditional (s, test) =
  case test of
    True -> addClass s
    False -> identity


{-| Add a list of class names to the current list contained in an `Element`
record

-}
addClassList : List String -> Element msg -> Element msg
addClassList ls =
  Internal.modify (\n -> { n | classes = List.append n.classes ls })


{-| Add a list of class names to the current list contained in an `Element`
record when the boolean argument evaluates to `True`

-}
addClassListConditional : (List String, Bool) -> Element msg -> Element msg
addClassListConditional (ls, test) =
  case test of
    True -> addClassList ls
    False -> identity


{-| Delete all instances of a class name from the current list contained in an
`Element` record

-}
removeClass : String -> Element msg -> Element msg
removeClass s =
  Internal.modify (\n -> { n | classes = n.classes |> List.filter ((/=) s) })


{-| Delete the current list of class names contained in current `Element`
record, replacing it with a new list of class names

-}
replaceClassList : List String -> Element msg -> Element msg
replaceClassList ls =
  Internal.modify (\n -> { n | classes = ls })


-- STYLE

{-| Add a style key/value pair to the current list contained in an `Element`
record

-}
addStyle : (String, String) -> Element msg -> Element msg
addStyle kv =
  Internal.modify (\n -> { n | styles = List.append n.styles [kv] })


{-| Add a style key/value pair to the current list contained in an `Element`
record when the boolean argument evaluates to `True`

-}
addStyleConditional : ((String, String), Bool) -> Element msg -> Element msg
addStyleConditional (kv, test) =
  case test of
    True -> addStyle kv
    False -> identity


{-| Add a list of style key/value pairs to the current list contained in an
`Element` record

-}
addStyleList : List (String, String) -> Element msg -> Element msg
addStyleList lkv =
  Internal.modify (\n -> { n | styles = List.append n.styles lkv })


{-| Add a list of style key/value pairs to the current list contained in an
`Element` record when the boolean argument evaluates to `True`

-}
addStyleListConditional : (List (String, String), Bool) -> Element msg -> Element msg
addStyleListConditional (lkv, test) =
  case test of
    True -> addStyleList lkv
    False -> identity


{-| Delete all instances of a style key from the current list contained in an
`Element` record

-}
removeStyle : String -> Element msg -> Element msg
removeStyle s =
  let
    isNotKey name (k, v) =
      k /= name

  in
    Internal.modify (\n -> { n | styles = n.styles |> List.filter (isNotKey s) })


{-| Delete the current list of style key/value pairs contained in current
`Element` record, replacing it with a new list of style key/value pairs

-}
replaceStyleList : List (String, String) -> Element msg -> Element msg
replaceStyleList lkv =
  Internal.modify (\n -> { n | styles = lkv })


-- EVENT LISTENERS

---- ACTIONS

addAction : (String, msg) -> Element msg -> Element msg
addAction (event, msg) =
  let
    handler =
      Json.Decode.succeed
        >> VirtualDom.Normal

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler msg) ] })


addActionConditional : ((String, msg), Bool) -> Element msg -> Element msg
addActionConditional (kv, test) =
  case test of
    True -> addAction kv
    False -> identity


addActionStopPropagation : (String, msg) -> Element msg -> Element msg
addActionStopPropagation (event, msg) =
  let
    handler =
      Json.Decode.succeed
        >> Json.Decode.map (\x -> (x, True))
        >> VirtualDom.MayStopPropagation

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler msg) ] })


addActionPreventDefault : (String, msg) -> Element msg -> Element msg
addActionPreventDefault (event, msg) =
  let
    handler =
      Json.Decode.succeed
        >> Json.Decode.map (\x -> (x, True))
        >> VirtualDom.MayPreventDefault

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler msg) ] })


addActionStopAndPrevent : (String, msg) -> Element msg -> Element msg
addActionStopAndPrevent (event, msg) =
  let
    handler =
      Json.Decode.succeed
        >> Json.Decode.map (\x ->
          { message = x
          , stopPropagation = True
          , preventDefault = True
          })
        >> VirtualDom.Custom

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler msg) ] })


---- INPUT HANDLERS

addInputHandler : (String -> msg) -> Element msg -> Element msg
addInputHandler token =
  let
    handler =
      Internal.captureStopPropagation ("value", Json.Decode.string)

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ ("input", handler token) ] })


addInputHandlerWithParser : (a -> msg, String -> a) -> Element msg -> Element msg
addInputHandlerWithParser (token, parser) =
  let
    handler =
      Internal.captureStopPropagation ("value", Json.Decode.string)

    transform =
      parser >> token

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ ("input", handler transform) ] })


addChangeHandler : (String -> msg) -> Element msg -> Element msg
addChangeHandler token =
  let
    handler =
      Internal.capture ("value", Json.Decode.string)

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ ("change", handler token) ] })


addChangeHandlerWithParser : (a -> msg, String -> a) -> Element msg -> Element msg
addChangeHandlerWithParser (token, parser) =
  let
    handler =
      Internal.capture ("value", Json.Decode.string)

    transform =
      parser >> token

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ ("change", handler transform) ] })


addCheckHandler : (Bool -> msg) -> Element msg -> Element msg
addCheckHandler token =
  let
    handler =
      Internal.capture ("checked", Json.Decode.bool)

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ ("change", handler token) ] })


---- CUSTOM LISTENERS

addListener : (String, Json.Decode.Decoder msg) -> Element msg -> Element msg
addListener (event, decoder) =
  let
    handler =
      VirtualDom.Normal

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler decoder) ] })


addListenerStopPropagation : (String, Json.Decode.Decoder msg) -> Element msg -> Element msg
addListenerStopPropagation (event, decoder) =
  let
    handler =
      Json.Decode.map (\d -> (d, True))
        >> VirtualDom.MayStopPropagation

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler decoder) ] })


addListenerPreventDefault : (String, Json.Decode.Decoder msg) -> Element msg -> Element msg
addListenerPreventDefault (event, decoder) =
  let
    handler =
      Json.Decode.map (\d -> (d, True))
        >> VirtualDom.MayPreventDefault

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler decoder) ] })


addListenerStopAndPrevent : (String, Json.Decode.Decoder msg) -> Element msg -> Element msg
addListenerStopAndPrevent (event, decoder) =
  let
    handler =
      Json.Decode.map (\x ->
        { message = x
        , stopPropagation = True
        , preventDefault = True
        }
      )
        >> VirtualDom.Custom

  in
    Internal.modify (\n -> { n | listeners = List.append n.listeners [ (event, handler decoder) ] })


removeListener : String -> Element msg -> Element msg
removeListener s =
  let
    isNotKey name (k, v) =
      k /= name

  in
    Internal.modify (\n -> { n | listeners = n.listeners |> List.filter (isNotKey s) })


-- -- add any other attribute using an `Html.Attribute` function or `VirtualDom` primitive
-- addAttribute : VirtualDom.Attribute msg -> Element msg -> Element msg
-- addAttributeConditional : (VirtualDom.Attribute msg, Bool) -> Element msg -> Element msg
-- addAttributeList : List (VirtualDom.Attribute msg) -> Element msg -> Element msg
-- addAttributeListConditional : (List (VirtualDom.Attribute msg), Bool) -> Element msg -> Element msg
-- replaceAttributeList : List (VirtualDom.Attribute msg) -> Element msg -> Element msg
--
-- -- Modifier functions for internal text:
-- -- Text is rendered to VirtualDom as the first child node, containing plain HTML text. Helper functions for styling text that were included in the previous version will be moved to a separate package.
--
-- appendText : String -> Element msg -> Element msg
-- appendTextConditional : (String, Bool) -> Element msg -> Element msg
-- prependText : String -> Element msg -> Element msg
-- prependTextConditional : (String, Bool) -> Element msg -> Element msg
-- replaceText : String -> Element msg -> Element msg
-- replaceTextConditional : (String, Bool) -> Element msg -> Element msg
--
-- -- Modifier functions to construct an element's internal tree:
-- -- Child elements are immediately rendered to VirtualDom when this function is executed. setChildListWithKeys uses the VirtualDom.KeyedNode optimization and should only be called once on any given node.
--
-- appendChild : Element msg -> Element msg -> Element msg
-- appendChildConditional : (Element msg, Bool) -> Element msg -> Element msg
-- appendChildList : List (Element msg) -> Element msg -> Element msg
-- appendChildListConditional : (List (Element msg), Bool) -> Element msg -> Element msg
-- appendNodeList : List (VirtualDom.Node msg) -> Element msg -> Element msg
-- prependChild : Element msg -> Element msg -> Element msg
-- prependChildConditional : (Element msg, Bool) -> Element msg -> Element msg
-- prependChildList : List (Element msg) -> Element msg -> Element msg
-- prependChildListConditional : (List (Element msg), Bool) -> Element msg -> Element msg
-- prependNodeList : List (VirtualDom.Node msg) -> Element msg -> Element msg
-- replaceChildList : List (Element msg) -> Element msg -> Element msg
-- replaceNodeList : List (VirtualDom.Node msg) -> Element msg -> Element msg
-- setChildListWithKeys : List (String, Element msg) -> Element msg -> Element msg
-- setNodeListWithKeys : List (String, VirtualDom.Node msg) -> Element msg -> Element msg
--
-- -- Modifier functions to set tag and namespace
-- -- The tag is already set with the element constructor function, but it could be useful to be able to change it when using component libraries. Namespace would typically be used to construct SVG nodes. Whenever the namespace field is not an empty string, VirtualDom.nodeNS is used for rendering.
--
-- setTag : String -> Element msg -> Element msg
-- setNamespace : String -> Element msg -> Element msg
--
-- -- Rendering
-- -- This function only needs to be called on the root node of a tree. VirtualDom.Node is interchangeable with Html.Html.
--
-- render : Element msg -> VirtualDom.Node msg
-- render root =
--   let
--     consId attributeList =
--       case root.id of
--         "" ->
--           attributeList
--
--         _ ->
--           ( root.id
--             |> Json.Encode.string
--             |> VirtualDom.property "id"
--           )
--             :: attributeList
--
--     consClassName attributeList =
--       case root.classes of
--         [] ->
--           attributeList
--
--         _ ->
--           ( root.classes
--             |> String.join " "
--             |> String.trim
--             |> Json.Encode.string
--             |> VirtualDom.property "className"
--           )
--             :: attributeList
--
--     consNamespace attributeList =
--       case root.namespace of
--         "" ->
--           attributeList
--
--         _ ->
--           ( root.namespace
--             |> Json.Encode.string
--             |> VirtualDom.property "namespace"
--           )
--             :: attributeList
--
--     consText childList =
--       case root.text of
--           "" ->
--             childList
--
--           _ ->
--             ( root.text
--               |> VirtualDom.text
--             )
--               :: childList
--
--     consTextKeyed keyedList =
--       case root.text of
--           "" ->
--             keyedList
--
--           _ ->
--             ( root.text
--               |> VirtualDom.text
--               |> (,) ("internal-text")
--             )
--               :: keyedList
--
--   in
--     case root.keys of
--       [] ->
--         root.children
--           |> consText
--           |> VirtualDom.node root.tag
--             ( root.attributes
--               |> consId
--               |> consClassName
--               |> consNamespace
--             )
--
--       _ ->
--         root.children
--           |> List.map2 (,) root.keys
--           |> consTextKeyed
--           |> VirtualDom.keyedNode root.tag
--             ( root.attributes
--               |> consId
--               |> consClassName
--               |> consNamespace
--             )


-- For debugging

getInternal : Element msg -> Internal.Data msg
getInternal n =
  case n of
    Internal.Element data -> data
