module Main exposing (..)

import Debug
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Url
import Url.Builder as Builder
import Url.Parser exposing (parse, query)
import Url.Parser.Query as Query

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt, resolve)

import Colors.Alpha as A
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..), Icon)
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes as SvgAtt
import Svg.Events as SvgEv
import UInt64 exposing (UInt64)

import Rule exposing (Rule, RuleIndex, flipBit, maxRuleIndex, numberOfCombinations)
import Combination exposing (Cell, Combination, getCombination)


-- MAIN


type alias Simulation =
    { index: Maybe UInt64
    , window: Maybe Int
    }


parseSimulation : Query.Parser Simulation
parseSimulation =
    Query.map2 Simulation (uint64 "index") (Query.int "window")


uint64 : String -> Query.Parser (Maybe UInt64)
uint64 key =
    Query.custom key <| \stringList ->
        case stringList of
            [str] ->
                Debug.log "parsing rule index" UInt64.fromString str

            _ ->
                Nothing


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


-- MODEL


type alias Model =
    { ruleIndex : RuleIndex
    , windowSize : Int
    , cellSize: Int
    , state : State
    , key : Nav.Key
    }


type State
    = Idle
    | Resuming
    | Running
    | Pausing


type AutomataAttribute
    = RuleIndex UInt64
    | WindowSize Int
    | CellSize Int
    | State State


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    let
        defaultRuleIndex : RuleIndex
        defaultRuleIndex =
            UInt64.fromInt 90

        defaultWindowSize : Int
        defaultWindowSize =
            3

        simulation : Simulation
        simulation =
            parse (query parseSimulation) url
                |> Maybe.withDefault
                    { index = Just defaultRuleIndex
                    , window = Just defaultWindowSize
                    }
    in
    ( { ruleIndex = simulation.index |> Maybe.withDefault defaultRuleIndex
      , windowSize = simulation.window |> Maybe.withDefault defaultWindowSize
      , cellSize = 5
      , state = Idle
      , key = key
      }
    , Cmd.none
    )


-- UPDATE


type Msg
    = UpdateWindowSize Int
    | UpdateCellSize Int
    | UpdateRuleIndex RuleIndex
    | ChangeState (Maybe State)
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateWindowSize newWindowSize ->
            let
                boundedRuleIndex : RuleIndex
                boundedRuleIndex =
                    sliceRuleIndex model.ruleIndex newWindowSize

                newUrl : String
                newUrl =
                    Builder.relative []
                        [ Builder.string "index" (UInt64.toString model.ruleIndex)
                        , Builder.int "window" newWindowSize
                        ]
            in
            ( model
            , Nav.pushUrl model.key newUrl
            )

        UpdateCellSize newCellSize ->
            ( { model | cellSize = newCellSize }
            , Cmd.none
            )

        UpdateRuleIndex newRuleIndex ->
            let
                newUrl : String
                newUrl =
                    Builder.relative []
                        [ Builder.string "index" (UInt64.toString newRuleIndex)
                        , Builder.int "window" model.windowSize
                        ]
            in
            ( model
            , Nav.pushUrl model.key newUrl
            )

        ChangeState stateValue ->
            case stateValue of
                Just newState ->
                    ( { model | state = newState }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        UrlChanged url ->
            let
                defaultRuleIndex : RuleIndex
                defaultRuleIndex =
                    UInt64.fromInt 110

                defaultWindowSize : Int
                defaultWindowSize =
                    3

                simulation : Simulation
                simulation =
                    parse (query parseSimulation) url
                        |> Maybe.withDefault
                            { index = Just defaultRuleIndex
                            , window = Just defaultWindowSize
                            }
            in
            ( { ruleIndex = simulation.index |> Maybe.withDefault defaultRuleIndex
              , windowSize = simulation.window |> Maybe.withDefault defaultWindowSize
              , cellSize = model.cellSize
              , state = Idle
              , key = model.key
              }
            , Cmd.none
            )


        LinkClicked urlRequest ->
            ( model
            , Cmd.none
            )


sliceRuleIndex : UInt64 -> Int -> UInt64
sliceRuleIndex ruleIndex windowSize =
    let
        mask : UInt64
        mask =
            maxRuleIndex windowSize
    in
    UInt64.and mask ruleIndex
            


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Simple Automata"
    , body =
        [ E.layout [] (viewAutomata model) ]
    }



viewAutomata : Model -> Element Msg
viewAutomata model =
    E.column
        [ E.centerX
        , E.width E.fill
        , E.height E.fill
        ]
        [ viewControls model
        , viewCombinations (Rule model.windowSize model.ruleIndex)
        , viewTape model
        ]


viewControls : Model -> Element Msg
viewControls model =
    let
        rule : Rule
        rule = Rule model.windowSize model.ruleIndex
    in
    E.row
        [ E.centerX
        , E.padding 10
        , E.spacingXY 20 0
        ]
        [ viewRuleIndexControls rule
        , viewWindowSizeControls rule
        , viewCellSizeControls model.cellSize
        , viewPlayControls model.state
        ]


viewPlayControls : State -> Element Msg
viewPlayControls state =
    let
        icon : Icon msg
        icon =
            case state of
                Idle ->
                    Filled.play_arrow

                Resuming ->
                    Filled.play_arrow

                Pausing ->
                    Filled.pause

                Running ->
                    Filled.pause

        action : Maybe Msg
        action =
            case state of
                Idle ->
                    Just (ChangeState (Just Resuming))

                Resuming ->
                    Nothing

                Running ->
                    Just (ChangeState (Just Pausing))

                Pausing ->
                    Nothing
    in
    Input.button []
        { onPress = action
        , label = E.html <| icon 32 Inherit
        }


viewRuleIndexControls : Rule -> Element Msg
viewRuleIndexControls rule =
    E.el
        [ E.width (E.px 300)
        , E.alignTop
        ] <|
            Input.text []
                { onChange = UInt64.fromString >> Maybe.withDefault UInt64.zero >> UpdateRuleIndex
                , text = UInt64.toString rule.ruleIndex
                , placeholder = Nothing
                , label = Input.labelAbove 
                    [ E.centerY
                    , Font.size 12
                    , tooltip E.below (viewTooltip ruleDescription)
                    ] <|
                        E.row
                            [ E.spaceEvenly
                            , E.width E.fill
                            ]
                            [ E.text ("Rule")
                            , E.html <| Filled.question_mark 12 Inherit
                            ]
                }


viewWindowSizeControls : Rule -> Element Msg
viewWindowSizeControls rule =
    E.el
        [ E.width (E.px 300) 
        , E.alignTop
        ] <|
            Input.slider
                [ E.height (E.px 30)
                , E.behindContent <|
                    -- Slider track
                    E.el
                        [ E.width E.fill
                        , E.height <| E.px 3
                        , E.centerY
                        , Background.color (A.lightcoral 1)
                        , Border.rounded 2
                        ]
                        E.none
                ]
                { onChange = round >> UpdateWindowSize
                , label = Input.labelAbove
                    [ E.centerY
                    , Font.size 12
                    , tooltip E.below (viewTooltip windowDescription)
                    ] <|
                        E.row
                            [ E.spaceEvenly
                            , E.width E.fill
                            ]
                            [ E.text ("Window size: " ++ (String.fromInt rule.windowSize))
                            , E.html <| Filled.question_mark 12 Inherit
                            ]
                , min = 1
                , max = 5
                , value = toFloat rule.windowSize
                , step = Just 2
                , thumb = Input.defaultThumb
                }


viewCellSizeControls : Int -> Element Msg
viewCellSizeControls cellSize  =
    E.el
        [ E.width (E.px 300) 
        , E.alignTop
        ] <|
            Input.slider
                [ E.height (E.px 30)
                , E.behindContent <|
                    -- Slider track
                    E.el
                        [ E.width E.fill
                        , E.height <| E.px 3
                        , E.centerY
                        , Background.color (A.lightcoral 1)
                        , Border.rounded 2
                        ]
                        E.none
                ]
                { onChange = round >> UpdateCellSize
                , label = Input.labelAbove
                    [ E.centerY
                    , Font.size 12
                    , tooltip E.below (viewTooltip cellDescription)
                    ] <|
                        E.row
                            [ E.spaceEvenly
                            , E.width E.fill
                            ]
                            [ E.text ("Cell size: " ++ (String.fromInt cellSize))
                            , E.html <| Filled.question_mark 12 Inherit
                            ]
                , min = 1
                , max = 15
                , value = toFloat cellSize
                , step = Just 1
                , thumb = Input.defaultThumb
                }


viewCombinations : Rule -> Element Msg
viewCombinations rule =
    let
        combinations : List (Element Msg)
        combinations =
            List.range 0 ((numberOfCombinations rule.windowSize) - 1)
                |> List.map (getCombination rule)
                |> List.indexedMap (\i c -> E.el [ E.centerX ] (drawCombination c 1 (UpdateRuleIndex (flipBit rule.ruleIndex (i + 1)))))
                |> List.reverse
    in
    E.row
        [ E.width E.fill
        , E.centerY
        , E.centerX
        , E.spacingXY 5 0
        , E.padding 10
        , E.scrollbarX
        , Background.color (A.lightcoral 1)
        ]
        combinations


drawCombination : Combination -> Int -> Msg -> Element Msg
drawCombination combination size msg =
    let
        combinationCells : List (Svg Msg)
        combinationCells =
            combination.neighborhood
                |> List.reverse
                |> List.indexedMap (\i cell -> drawCell 0 i cell)

        windowMiddleIndex : Int
        windowMiddleIndex =
            List.length combinationCells // 2

        outcomeCell : Svg Msg
        outcomeCell =
            drawCell 1 windowMiddleIndex combination.outcome

        drawWidth : Int
        drawWidth =
            getOffset (List.length combinationCells) + 2

        drawHeight : Int
        drawHeight =
            (getOffset 2) + 2
    in
    E.html <|
        svg
            [ SvgAtt.width (String.fromInt (size * drawWidth))
            , SvgAtt.height (String.fromInt (size * drawHeight))
            , SvgAtt.viewBox ("-1 -1 " ++ (String.fromInt drawWidth) ++ " " ++ (String.fromInt drawHeight))
            , SvgEv.onClick msg
            ]
            [ g [] combinationCells
            , outcomeCell
            ]


drawCell : Int -> Int -> Cell -> Svg Msg
drawCell row column cell =
    let
        xOffset : Int
        xOffset = getOffset column

        yOffset : Int
        yOffset = getOffset row

        fillColor : String
        fillColor = getCellColor cell

        attributes : List (Svg.Attribute Msg)
        attributes =
            [ SvgAtt.x (String.fromInt xOffset)
            , SvgAtt.y (String.fromInt yOffset)
            , SvgAtt.width (String.fromInt combinationCellSize)
            , SvgAtt.height (String.fromInt combinationCellSize)
            , SvgAtt.fill fillColor
            , SvgAtt.stroke "black"
            , SvgAtt.strokeWidth (String.fromInt (cellSpacing + 1))
            ]
    in
    rect attributes []


viewTape : Model -> Element Msg
viewTape model =
    E.el
        [ E.width E.fill
        , E.height E.fill
        ]
        ( E.html <|
            viewCustomElement
                [ mapCustomAttribute (RuleIndex model.ruleIndex)
                , mapCustomAttribute (WindowSize model.windowSize)
                , mapCustomAttribute (State model.state)
                , mapCustomAttribute (CellSize model.cellSize)
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , onStateChange
                ]
                [ Html.canvas
                    [ Html.Attributes.id "ca-canvas"
                    ]
                    []
                , Html.canvas
                    [ Html.Attributes.id "ca-counts"
                    ]
                    []
                ]
        )


viewCustomElement : List (Html.Attribute a) -> List (Html a) -> Html a
viewCustomElement =
    Html.node "cellular-automata"


onStateChange : Html.Attribute Msg
onStateChange =
    stateDecoder
        |> Json.Decode.map ChangeState
        |> on "state-change"


stateDecoder : Decoder (Maybe State)
stateDecoder =
    let
        mapState : String -> Maybe State
        mapState stateString =
            case stateString of
                "idle" ->
                    Just Idle

                "running" ->
                    Just Running

                _ ->
                    Nothing

        toDecoder : String -> Decoder (Maybe State)
        toDecoder state =
            Json.Decode.succeed (mapState state)
    in
    Json.Decode.succeed toDecoder
        |> requiredAt [ "detail", "state" ] Json.Decode.string
        |> resolve


mapCustomAttribute : AutomataAttribute -> Html.Attribute a
mapCustomAttribute att =
    let
        fields : { field : String, value : String }
        fields =
            case att of
                RuleIndex ruleIndex ->
                    { field = "rule-index", value = UInt64.toString ruleIndex }

                WindowSize windowSize ->
                    { field = "window-size", value = String.fromInt windowSize }

                CellSize cellSize ->
                    { field = "cell-size", value = String.fromInt cellSize }

                State state ->
                    let
                        command : String
                        command =
                            case state of
                                Idle ->
                                    "pause"

                                Pausing ->
                                    "pause"

                                Running ->
                                    "resume"

                                Resuming ->
                                    "resume"
                    in
                    { field = "desired-state", value = command }
    in
    Html.Attributes.attribute fields.field fields.value


getOffset : Int -> Int
getOffset index =
    combinationCellSize * index + cellSpacing * (index + 1)


combinationCellSize : Int
combinationCellSize = 15

cellSpacing : Int
cellSpacing = 0


getCellColor : Cell -> String
getCellColor cell =
    case cell of
        Combination.Alive ->
            "black"

        Combination.Dead ->
            "white"


viewTooltip : String -> Element msg
viewTooltip tooltip_ =
    E.el
        [ Background.color (A.black 1)
        , Font.color (A.white 1)
        , E.padding 4
        , Border.rounded 4
        , Font.size 12
        ]
        (E.text tooltip_)


tooltip : (Element msg -> E.Attribute msg) -> Element Never -> E.Attribute msg
tooltip usher tooltip_ =
    E.inFront <|
        E.el
            [ E.width E.fill
            , E.height E.fill
            , E.transparent True
            , E.mouseOver [ E.transparent False ]
            , (usher << E.map never) <|
                E.el [ E.htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltip_
            ]
            E.none


ruleDescription : String
ruleDescription =
    """
    The rule can be set here with an integer or you can click on the
    combinations row below activating/deactivating individual
    combinations. The rule index cannot exceed 2 ^ (window size).
    
    The combinations determine which cells in the next iteration will
    become active.
    """


windowDescription : String
windowDescription =
    """
    The window size determines how many neighbors of the cell will be
    taken into account when calculating the next cell iteration. For
    a window size of 3, the cell will consider itself and the immediate
    neighbor to the left and right.
    """


cellDescription : String
cellDescription =
    """
    Set the size of the cells displayed below. Bigger cells means there
    will be less cells in the canvas below, but the visibility will be
    better.
    """
