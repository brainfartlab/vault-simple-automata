module Rule exposing (..)

import Bitwise
import UInt64 exposing (UInt64)

type alias Rule =
    { windowSize : Int
    , ruleIndex : RuleIndex
    }


type alias RuleIndex = UInt64


maxRuleIndex : Int -> UInt64
maxRuleIndex windowSize =
    let
        upperBound : UInt64
        upperBound =
            UInt64.pow (UInt64.fromInt 2) (UInt64.fromInt (numberOfCombinations windowSize))
    in
    UInt64.sub upperBound  UInt64.one


numberOfCombinations : Int -> Int
numberOfCombinations windowSize =
    2 ^ windowSize


toggleCombination : Rule -> Int -> Rule
toggleCombination rule combinationIndex =
    let
        newRuleIndex : UInt64
        newRuleIndex =
            flipBit rule.ruleIndex (combinationIndex + 1)
    in
    Rule rule.windowSize newRuleIndex


flipBit : UInt64 -> Int -> UInt64
flipBit number position =
    let
        shift : Int
        shift = position - 1
    in
    UInt64.xor (UInt64.shiftLeftBy shift (UInt64.fromInt 1)) number
