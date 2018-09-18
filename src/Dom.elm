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


{-| A record containing all of the data needed to construct an HTML node (via
`VirtualDom.Node`). By using a record to temporarily store data about a node,
we can partially construct that node with some data, but delay building it until
all of the data has been assembled. In this way, all of a node's data is
available to be modified until it is either placed in a container element or
passed as an argument to the `render` function.

-}
type Element msg =
  Element (Internal msg)


{-| Specification of the internal record type. This is not exposed.

-}
type alias Internal msg =
  { tag : String
  , id : String
  , classes : List String
  , styles : List (String, String)
  , actions: List (String, Handler msg)
  , attributes : List (VirtualDom.Attribute msg)
  , text : String
  , children : List (VirtualDom.Node msg)
  , namespace : String
  , keys : List String
  }


{-| Options for custom event handlers in VirtualDom

-}
type alias Handler msg =
  { message : msg
  , stopPropagation : Bool
  , preventDefault : Bool
  }


-- CONSTRUCTOR

{-| Constructor for `Element` records. The string argument provides the HTML
tag.

-}
element : String -> Element msg
element tag =
  Element
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


{-| This function is used to access an `Element`'s internal record when applying
a modifier function. By using it, we can avoid writing a case expression for
each of the exposed functions below. This is an alternative to placing the
implementation code in an separate, unexposed module.

-}
modify : (Internal msg -> Internal msg) -> Element msg -> Element msg
modify f n =
  case n of
    Element i -> Element (f i)


-- ID

{-| Set the id attribute on an `Element` record
-}
setId : String -> Element msg -> Element msg
setId s =
  modify (\n -> { n | id = s })


-- CLASS

{-| Add a class name to the current list contained in an `Element` record

-}
addClass : String -> Element msg -> Element msg
addClass s =
  modify (\n -> { n | classes = List.append n.classes [s] })


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
  modify (\n -> { n | classes = List.append n.classes ls })


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
  modify (\n -> { n | classes = n.classes |> List.filter ((/=) s) })


{-| Delete the current list of class names contained in current `Element`
record, replacing it with a new list of class names

-}
replaceClassList : List String -> Element msg -> Element msg
replaceClassList ls =
  modify (\n -> { n | classes = ls })


-- STYLE

{-| Add a style key/value pair to the current list contained in an `Element`
record

-}
addStyle : (String, String) -> Element msg -> Element msg
addStyle kv =
  modify (\n -> { n | styles = List.append n.styles [ kv ] })


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
  modify (\n -> { n | styles = List.append n.styles lkv })


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
    isNotKey arg (k, v) =
      k /= arg

  in
    modify (\n -> { n | styles = n.styles |> List.filter (isNotKey s) })


{-| Delete the current list of style key/value pairs contained in current
`Element` record, replacing it with a new list of style key/value pairs

-}
replaceStyleList : List (String, String) -> Element msg -> Element msg
replaceStyleList lkv =
  modify (\n -> { n | styles = lkv })


-- ACTION
---- event listeners that don't capture an input value

addAction : (String, msg) -> Element msg -> Element msg
addAction (k, v) =
  let
    newAction =
      (k, Handler v False False)

  in
    modify (\n -> { n | actions = List.append n.actions [ newAction ] })


addActionConditional : ((String, msg), Bool) -> Element msg -> Element msg
addActionConditional (kv, test) =
  case test of
    True -> addAction kv
    False -> identity


addActionStopPropagation : (String, msg) -> Element msg -> Element msg
addActionStopPropagation (k, v) =
  let
    newAction =
      (k, Handler v True False)

  in
    modify (\n -> { n | actions = List.append n.actions [ newAction ] })


addActionPreventDefault : (String, msg) -> Element msg -> Element msg
addActionPreventDefault (k, v) =
  let
    newAction =
      (k, Handler v False True)

  in
    modify (\n -> { n | actions = List.append n.actions [ newAction ] })


addActionStopAndPrevent : (String, msg) -> Element msg -> Element msg
addActionStopAndPrevent (k, v) =
  let
    newAction =
      (k, Handler v True True)

  in
    modify (\n -> { n | actions = List.append n.actions [ newAction ] })


removeAction : String -> Element msg -> Element msg
removeAction s =
  let
    isNotKey arg (k, v) =
      k /= arg

  in
    modify (\n -> { n | actions = n.actions |> List.filter (isNotKey s) })


-- event listeners that capture an input value (immediately rendered to `VirtualDom.Attribute`)


captureValue : (String, String -> Handler msg) -> VirtualDom.Attribute msg
captureValue (event, receiver) =
  let
    handler =
      Json.Decode.string
        |> Json.Decode.at [ "target", "value" ]
        |> Json.Decode.map receiver
        |> VirtualDom.Custom

  in
    VirtualDom.on event handler


captureChecked : (String, Bool -> Handler msg) -> VirtualDom.Attribute msg
captureChecked (event, receiver) =
  let
    handler =
      Json.Decode.bool
        |> Json.Decode.at [ "target", "checked" ]
        |> Json.Decode.map receiver
        |> VirtualDom.Custom

  in
    VirtualDom.on event handler


setInputHandler : (String -> msg) -> Element msg -> Element msg
setInputHandler token =
  let
    receiver string =
      Handler (token string) True False

    newAttribute =
      captureValue ("input", receiver)

  in
    modify (\n -> { n | attributes = List.append n.attributes [ newAttribute ] })


setInputHandlerWithParser : (a -> msg, String -> a) -> Element msg -> Element msg
setInputHandlerWithParser (token, parser) =
  let
    receiver string =
      Handler (string |> parser |> token) True False

    newAttribute =
      captureValue ("input", receiver)

  in
    modify (\n -> { n | attributes = List.append n.attributes [ newAttribute ] })


setChangeHandler : (String -> msg) -> Element msg -> Element msg
setChangeHandler token =
  let
    receiver string =
      Handler (token string) False False

    newAttribute =
      captureValue ("change", receiver)

  in
    modify (\n -> { n | attributes = List.append n.attributes [ newAttribute ] })


setChangeHandlerWithParser : (a -> msg, String -> a) -> Element msg -> Element msg
setChangeHandlerWithParser (token, parser) =
  let
    receiver string =
      Handler (string |> parser |> token) False False

    newAttribute =
      captureValue ("change", receiver)

  in
    modify (\n -> { n | attributes = List.append n.attributes [ newAttribute ] })


setCheckHandler : (Bool -> msg) -> Element msg -> Element msg
setCheckHandler token =
  let
    receiver bool =
      Handler (token bool) False False

    newAttribute =
      captureChecked ("change", receiver)

  in
    modify (\n -> { n | attributes = List.append n.attributes [ newAttribute ] })



-- setCustomHandler : (String, Decoder msg) -> Element msg -> Element msg
-- setCustomHandlerStopPropagation : (String, Decoder msg) -> Element msg -> Element msg
-- setCustomHandlerPreventDefault : (String, Decoder msg) -> Element msg -> Element msg
-- setCustomHandlerStopAndPrevent : (String, Decoder msg) -> Element msg -> Element msg
--
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

getInternal : Element msg -> Internal msg
getInternal n =
  case n of
    Element i -> i
