module Dom.Internal exposing
  ( Element(..)
  , Data
  , render
  , modify
  , capture
  , captureStopPropagation
  , capturePreventDefault
  , captureStopAndPrevent
  )

{-| This module is exposed so that package developers can make use of element
record internals and helper functions. It is not recommended for use in
application code.
-}

import VirtualDom
import Json.Decode

{-| Abstraction of an
[Element](https://developer.mozilla.org/en-US/docs/Web/API/Element) in the
[Document Object Model](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
(DOM) interface to HTML and XML documents.

A record containing all of the data needed to construct an HTML node (via
`VirtualDom.Node`). By using a record to temporarily store data about a node,
we can partially construct that node with some data, but delay building it until
all of the data has been assembled. In this way, all of a node's data is
available to be modified until it is either placed in a container element or
passed as an argument to the `render` function.

-}
type Element msg =
  Element (Data msg)


{-| Internal data

-}
type alias Data msg =
  { tag : String
  , id : String
  , classes : List String
  , styles : List (String, String)
  , attributes : List (VirtualDom.Attribute msg)
  , listeners: List (String, VirtualDom.Handler msg)
  , text : String
  , children : List (VirtualDom.Node msg)
  , namespace : String
  , keys : List String
  }


{-| This function is used to access an `Element`'s internal record when applying
a modifier function. By using it, we can avoid writing a case expression for
each of the exposed functions below. This is an alternative to placing the
implementation code in an separate, unexposed module.

-}
modify : (Data msg -> Data msg) -> Element msg -> Element msg
modify f n =
  case n of
    Element data ->
      Element (f data)


render : Element msg -> VirtualDom.Node msg
render =
  always (VirtualDom.node "div" [] [])

capture : (String, Json.Decode.Decoder a) -> (a -> msg) -> VirtualDom.Handler msg
capture (field, decoder) token =
  decoder
    |> Json.Decode.at ["target", field]
    |> Json.Decode.map token
    |> VirtualDom.Normal


captureStopPropagation : (String, Json.Decode.Decoder a) -> (a -> msg) -> VirtualDom.Handler msg
captureStopPropagation (field, decoder) token =
  decoder
    |> Json.Decode.at ["target", field]
    |> Json.Decode.map token
    |> Json.Decode.map (\x -> (x, True))
    |> VirtualDom.MayStopPropagation


capturePreventDefault : (String, Json.Decode.Decoder a) -> (a -> msg) -> VirtualDom.Handler msg
capturePreventDefault (field, decoder) token =
  decoder
    |> Json.Decode.at ["target", field]
    |> Json.Decode.map token
    |> Json.Decode.map (\x -> (x, True))
    |> VirtualDom.MayPreventDefault


captureStopAndPrevent : (String, Json.Decode.Decoder a) -> (a -> msg) -> VirtualDom.Handler msg
captureStopAndPrevent (field, decoder) token =
  decoder
    |> Json.Decode.at ["target", field]
    |> Json.Decode.map token
    |> Json.Decode.map (\x ->
      { message = x
      , stopPropagation = True
      , preventDefault = True
      })
    |> VirtualDom.Custom



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
