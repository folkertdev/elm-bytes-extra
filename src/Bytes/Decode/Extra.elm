module Bytes.Decode.Extra exposing
    ( iterate
    , map8, map16
    , unsignedInt24, signedInt24
    , list, array, dict, set
    )

{-| Extra decoding functions

@docs iterate
@docs map8, map16
@docs unsignedInt24, signedInt24


# Common data structures

@docs list, array, dict, set

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)
import Set exposing (Set)


{-| Repeat a decoder a given number of times
-}
iterate : Int -> (state -> Decoder state) -> state -> Decoder state
iterate n step initial =
    Decode.loop ( n, initial ) (iterateHelp step)


iterateHelp step ( n, state ) =
    if n > 0 then
        step state
            |> Decode.map (\new -> Loop ( n - 1, new ))

    else
        Decode.succeed (Decode.Done state)



-- DATA STRUCTURES


{-| Decode a list of given length
-}
list : Int -> Decoder a -> Decoder (List a)
list n decoder =
    Decode.loop ( n, [] ) (listHelp decoder)


listHelp element ( n, accum ) =
    if n > 0 then
        Decode.map (\new -> Loop ( n - 1, new :: accum )) element

    else
        Decode.succeed (Decode.Done (List.reverse accum))


{-| Decode an array of given length
-}
array : Int -> Decoder a -> Decoder (Array a)
array n decoder =
    Decode.loop ( n, Array.empty ) (arrayHelp decoder)


arrayHelp element ( n, accum ) =
    if n > 0 then
        Decode.map (\new -> Loop ( n - 1, Array.push new accum )) element

    else
        Decode.succeed (Decode.Done accum)


{-| Decode a set of given size

The size is a maximum because sets don't add duplicates

-}
set : Int -> Decoder comparable -> Decoder (Set comparable)
set n decoder =
    Decode.loop ( n, Set.empty ) (setHelp decoder)


setHelp element ( n, accum ) =
    if n > 0 then
        Decode.map (\new -> Loop ( n - 1, Set.insert new accum )) element

    else
        Decode.succeed (Decode.Done accum)


{-| Decode a dictionary of given size

The size is a maximum because dictionaries don't add duplicates

-}
dict : Int -> Decoder comparable -> Decoder value -> Decoder (Dict comparable value)
dict n keyDecoder valueDecoder =
    Decode.loop ( n, Dict.empty ) (dictHelp keyDecoder valueDecoder)


dictHelp keyDecoder valueDecoder ( n, accum ) =
    if n > 0 then
        Decode.map2 (\key value -> Loop ( n - 1, Dict.insert key value accum )) keyDecoder valueDecoder

    else
        Decode.succeed (Decode.Done accum)



-- MAPS


{-| Map a function over 8 values at once
-}
map8 :
    (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> b7 -> b8 -> result)
    -> Decoder b1
    -> Decoder b2
    -> Decoder b3
    -> Decoder b4
    -> Decoder b5
    -> Decoder b6
    -> Decoder b7
    -> Decoder b8
    -> Decoder result
map8 f b1 b2 b3 b4 b5 b6 b7 b8 =
    let
        d1 =
            Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Decode.map5 (\h a b c d -> h a b c d) d1 b5 b6 b7 b8
    in
    d2


{-| Map a function over 16 values at once
-}
map16 :
    (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> b7 -> b8 -> b9 -> b10 -> b11 -> b12 -> b13 -> b14 -> b15 -> b16 -> result)
    -> Decoder b1
    -> Decoder b2
    -> Decoder b3
    -> Decoder b4
    -> Decoder b5
    -> Decoder b6
    -> Decoder b7
    -> Decoder b8
    -> Decoder b9
    -> Decoder b10
    -> Decoder b11
    -> Decoder b12
    -> Decoder b13
    -> Decoder b14
    -> Decoder b15
    -> Decoder b16
    -> Decoder result
map16 f b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 =
    let
        d1 =
            Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Decode.map5 (\h a b c d -> h a b c d) d1 b5 b6 b7 b8

        d3 =
            Decode.map5 (\h a b c d -> h a b c d) d2 b9 b10 b11 b12

        d4 =
            Decode.map5 (\h a b c d -> h a b c d) d3 b13 b14 b15 b16
    in
    d4



-- 24-bit


{-| Decode a signed 24-bit integer
-}
signedInt24 : Endianness -> Decoder Int
signedInt24 endianness =
    let
        recombine b16 byte3 =
            let
                byte1 =
                    Bitwise.and 0xFF (Bitwise.shiftRightZfBy 8 b16)

                byte2 =
                    Bitwise.and 0xFF b16
            in
            case endianness of
                BE ->
                    let
                        sign =
                            if b16 < 0 then
                                -1

                            else
                                1
                    in
                    Bitwise.or byte1 (Bitwise.or byte2 byte3)
                        |> abs
                        |> (*) sign

                LE ->
                    let
                        sign =
                            if byte3 < 0 then
                                -1

                            else
                                1
                    in
                    Bitwise.or byte3 (Bitwise.or byte2 byte1)
                        |> abs
                        |> (*) sign
    in
    Decode.map2 recombine (Decode.signedInt16 endianness) Decode.unsignedInt8


{-| Decode an unsigned 24-bit integer
-}
unsignedInt24 : Endianness -> Decoder Int
unsignedInt24 endianness =
    let
        recombine b16 byte3 =
            let
                byte1 =
                    Bitwise.and 0xFF (Bitwise.shiftRightZfBy 8 b16)

                byte2 =
                    Bitwise.and 0xFF b16
            in
            case endianness of
                BE ->
                    Bitwise.or byte1 (Bitwise.or byte2 byte3)

                LE ->
                    Bitwise.or byte3 (Bitwise.or byte2 byte1)
    in
    Decode.map2 recombine (Decode.unsignedInt16 endianness) Decode.unsignedInt8
