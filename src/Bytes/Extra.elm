module Bytes.Extra exposing
    ( empty
    , slice, take, drop
    , toByteValues, fromByteValues
    )

{-| Extra functions on `Bytes`.

@docs empty
@docs slice, take, drop

@docs toByteValues, fromByteValues

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)



-- byte values


{-| Turn `Bytes` into a list of byte values

This decodes 32-bit integers, then splits them up into their four constituent bytes. This is faster than decoding individual bytes.

-}
toByteValues : Bytes -> List Int
toByteValues buffer =
    case Decode.decode (Decode.loop ( Bytes.width buffer, [] ) toByteValuesHelp) buffer of
        Just v ->
            v

        Nothing ->
            []


toByteValuesHelp : ( Int, List Int ) -> Decoder (Step ( Int, List Int ) (List Int))
toByteValuesHelp ( remaining, accum ) =
    if remaining >= 4 then
        Decode.unsignedInt32 BE
            |> Decode.map
                (\value ->
                    let
                        b1 =
                            value
                                |> Bitwise.shiftRightZfBy 24
                                |> Bitwise.and 0xFFFF

                        b2 =
                            value
                                |> Bitwise.shiftRightZfBy 16
                                |> Bitwise.and 0xFFFF

                        b3 =
                            value
                                |> Bitwise.shiftRightZfBy 8
                                |> Bitwise.and 0xFFFF

                        b4 =
                            value
                                |> Bitwise.and 0xFFFF
                    in
                    Decode.Loop ( remaining - 4, b1 :: b2 :: b3 :: b4 :: accum )
                )

    else if remaining > 0 then
        Decode.unsignedInt8
            |> Decode.map
                (\value ->
                    Decode.Loop ( remaining - 1, value :: accum )
                )

    else
        Decode.succeed (Decode.Done (List.reverse accum))


{-| Turn `Bytes` into a list of byte values

This function takes four bytes and encodes them as a 32-bit integer. This is MUCH faster than the naive approach.

-}
fromByteValues : List Int -> Bytes
fromByteValues values =
    fromByteValuesHelp values []
        |> Encode.sequence
        |> Encode.encode


fromByteValuesHelp : List Int -> List Encoder -> List Encoder
fromByteValuesHelp values encoders =
    case values of
        x :: y :: z :: w :: rest ->
            fromByteValuesHelp rest (packInt32 x y z w :: encoders)

        x :: rest ->
            fromByteValuesHelp rest (Encode.unsignedInt8 x :: encoders)

        [] ->
            List.reverse encoders


packInt32 : Int -> Int -> Int -> Int -> Encoder
packInt32 a b c d =
    Encode.unsignedInt32 BE <|
        Bitwise.or
            (Bitwise.or (Bitwise.shiftLeftBy 24 (Bitwise.and 0xFFFF a))
                (Bitwise.shiftLeftBy 16 (Bitwise.and 0xFFFF b))
            )
            (Bitwise.or
                (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFFFF c))
                (Bitwise.and 0xFFFF d)
            )


{-| An empty `Bytes`. Useful for default cases or unreachable branches.
-}
empty : Bytes
empty =
    Encode.encode (Encode.sequence [])


{-| Slice a segment from a `Bytes`

    buffer : Bytes
    buffer = fromByteValues (List.range 0 20)

    buffer
        |> slice 5 10
        |> toByteValues
        --> [ 5, 6,7,8,9 ]

-}
slice : Int -> Int -> Bytes -> Bytes
slice from to buffer =
    let
        toKeep =
            min (to - from) (Bytes.width buffer - to)

        decoder =
            Decode.map2 (\_ v -> v)
                (Decode.bytes from)
                (Decode.bytes toKeep)
    in
    case Decode.decode decoder buffer of
        Just v ->
            v

        Nothing ->
            empty


{-| Take a prefix of a `Bytes`

    buffer : Bytes
    buffer = fromByteValues (List.range 0 20)

    buffer
        |> take 5
        |> toByteValues
        --> [ 0,1,2,3,4 ]

-}
take : Int -> Bytes -> Bytes
take size buffer =
    case Decode.decode (Decode.bytes size) buffer of
        Just v ->
            v

        Nothing ->
            buffer


{-| Take a suffix of a `Bytes`

    buffer : Bytes
    buffer = fromByteValues (List.range 0 20)

    buffer
        |> drop 15
        |> toByteValues
        --> [ 15, 16, 17, 18, 19, 20 ]

-}
drop : Int -> Bytes -> Bytes
drop size buffer =
    let
        toKeep =
            Bytes.width buffer - size
    in
    if toKeep <= 0 then
        empty

    else
        let
            decoder =
                Decode.map2 (\_ v -> v)
                    (Decode.bytes size)
                    (Decode.bytes toKeep)
        in
        case Decode.decode decoder buffer of
            Just v ->
                v

            Nothing ->
                empty
