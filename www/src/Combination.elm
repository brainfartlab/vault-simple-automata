module Combination exposing (Combination, Cell(..), getCombination)

import Bitwise
import Rule exposing (Rule)
import UInt64 exposing (UInt64)

type alias Combination =
    { neighborhood : List Cell
    , outcome : Cell
    }

type Cell
    = Alive
    | Dead


isBitActive : UInt64 -> Int -> Bool
isBitActive number position =
    let
        shift : Int
        shift = position - 1

        mask : UInt64
        mask = UInt64.shiftLeftBy shift UInt64.one
    in
    (UInt64.and mask number) /= UInt64.zero


cellFromFlag : Bool -> Cell
cellFromFlag flag =
    if flag then
        Alive
    else
        Dead


getCombination : Rule -> Int -> Combination
getCombination rule combinationIndex =
    let
        neighborhood : List Cell
        neighborhood =
            List.range 1 rule.windowSize
                |> List.map (isBitActive (UInt64.fromInt combinationIndex))
                |> List.map cellFromFlag
    in
    { neighborhood = neighborhood
    , outcome = cellFromFlag (isBitActive rule.ruleIndex (combinationIndex + 1))
    }
