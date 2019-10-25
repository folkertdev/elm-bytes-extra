module Bytes.Encode.Extra exposing
    ( empty
    , signedInt24, unsignedInt24
    , listWithSize, arrayWithSize, stringWithSize, setWithSize, dictWithSize
    )

{-| Extra encoding functions

@docs empty


## 24-bit integers

@docs signedInt24, unsignedInt24


## Sized collections

Encode collections preceded with their size, for easy decoding. If you don't want to encode the size, `empty` is helpful:

    -- list without size
    list :
        (a -> Encoder)
        -> List a
        -> Encoder
    list =
        listWithSize (\_ -> Bytes.Encode.Extra.empty)

@docs listWithSize, arrayWithSize, stringWithSize, setWithSize, dictWithSize

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import Set exposing (Set)


{-| An empty encoder
-}
empty : Encoder
empty =
    Encode.sequence []


{-| Encode a 24-bit signed integer
-}
signedInt24 : Int -> Endianness -> Encoder
signedInt24 n endianness =
    let
        b12 =
            n |> Bitwise.shiftRightZfBy 8 |> Bitwise.and 0xFFFF

        b3 =
            n |> Bitwise.and 0xFF
    in
    case endianness of
        BE ->
            Encode.sequence [ Encode.signedInt16 BE b12, Encode.unsignedInt8 b3 ]

        LE ->
            Encode.sequence [ Encode.unsignedInt8 b3, Encode.signedInt16 LE b12 ]


{-| Encode a 24-bit unsigned integer
-}
unsignedInt24 : Int -> Endianness -> Encoder
unsignedInt24 n endianness =
    let
        b12 =
            n |> Bitwise.shiftRightZfBy 8 |> Bitwise.and 0xFFFF

        b3 =
            n |> Bitwise.and 0xFF
    in
    case endianness of
        BE ->
            Encode.sequence [ Encode.unsignedInt16 BE b12, Encode.unsignedInt8 b3 ]

        LE ->
            Encode.sequence [ Encode.unsignedInt8 b3, Encode.unsignedInt16 LE b12 ]


{-| Encode a list with its size
-}
listWithSize : (Int -> Encoder) -> (a -> Encoder) -> List a -> Encoder
listWithSize toSize toEncoder values =
    List.map toEncoder values
        |> (::) (toSize (List.length values))
        |> Encode.sequence


{-| Encode an array with its size
-}
arrayWithSize : (Int -> Encoder) -> (a -> Encoder) -> Array a -> Encoder
arrayWithSize toSize toEncoder values =
    Array.foldr (\elem accum -> toEncoder elem :: accum) [] values
        |> (::) (toSize (Array.length values))
        |> Encode.sequence


{-| Encode a set with its size
-}
setWithSize : (Int -> Encoder) -> (a -> Encoder) -> Set a -> Encoder
setWithSize toSize toEncoder values =
    Set.foldr (\elem accum -> toEncoder elem :: accum) [] values
        |> (::) (toSize (Set.size values))
        |> Encode.sequence


{-| Encode a string with its size
-}
stringWithSize : (Int -> Encoder) -> String -> Encoder
stringWithSize toSize value =
    Encode.sequence
        [ toSize (String.length value)
        , Encode.string value
        ]


{-| Encode a dictionary with its size
-}
dictWithSize : (Int -> Encoder) -> (key -> Encoder) -> (value -> Encoder) -> Dict key value -> Encoder
dictWithSize toSize toKey toValue values =
    Dict.foldr (\key value accum -> toKey key :: toValue value :: accum) [] values
        |> (::) (toSize (Dict.size values))
        |> Encode.sequence
