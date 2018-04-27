import Html exposing (Html, div, button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond, second)
import Array exposing (Array)
import Simulator exposing (State, step)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Markdown

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
    { k: Int
    , stepsToGo : Int
    , current: Array State
    , frameRate : Time
    , trace : Maybe (Html Msg)
    , isPlaying : Bool
    }

init : (Model, Cmd Msg)
init =
    let
        i  = 3
        ps = Simulator.init i
    in
        (Model i (Simulator.numSteps ps) ps millisecond Nothing True, Cmd.none)

type Msg
    = Tick Time
    | KChange String
    | TimeChange String
    | ToggleTrace
    | Restart
    | TogglePlaying

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick    _ -> if model.stepsToGo >= 1 && model.isPlaying
        then ({ model | stepsToGo = model.stepsToGo - 1, current = Simulator.step model.current}, Cmd.none)
        else (model, Cmd.none)
    KChange k ->
        let
            i  = String.toInt k |> Result.withDefault 3
            ps = Simulator.init i
        in
            ({model | k = i, stepsToGo = Simulator.numSteps ps, current = ps, trace = Nothing}, Cmd.none)
    TimeChange t ->
        let
            tc = 11 - (Result.withDefault 5.0 <| String.toFloat t)
        in
            ({model | frameRate = tc / toFloat (Simulator.numSteps model.current)}, Cmd.none)
    ToggleTrace ->
        let
            newTrace =
                if model.trace == Nothing && model.k <= 7
                    then Just <| div [] [tracer model.k]
                    else Nothing
        in
            ({model | trace = newTrace }, Cmd.none)
    Restart ->
        let
            ps = Simulator.init model.k
        in
            ({model | stepsToGo = Simulator.numSteps ps, current = ps}, Cmd.none)
    TogglePlaying -> ({model | isPlaying = not model.isPlaying}, Cmd.none)

-- NOTE: This shouldn't really be used for animation since the browser has it's
--       own loop, but we're not looking for flawless graphics here.
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (model.frameRate * second) Tick

directionalTxt : Simulator.Direction -> String -> String
directionalTxt d txt = case d of
    Simulator.TowardsRight -> txt ++ ">"
    Simulator.TowardsLeft -> "<" ++ txt

styleFn : State -> (String, String)
styleFn s = case s of
    Simulator.Null         -> ("#EFEFEF", "")
    Simulator.Go           -> ("#8659A4", "GO")
    Simulator.Wait1 d      -> ("#F48807", directionalTxt d "1")
    Simulator.Wait2 d      -> ("#D85408", directionalTxt d "2")
    Simulator.Ping d       -> ("#9FCFDB", directionalTxt d "P")
    Simulator.SlowPing d   -> ("#F4B80F", directionalTxt d "0")
    Simulator.ReturnPing d -> ("#5C69A4", directionalTxt d "R")
    Simulator.Middle       -> ("#E1F400", "PP")
    Simulator.PreTada      -> ("#F1F400", "..")
    Simulator.Tada         -> ("#00A46F", ":)")
    Simulator.Wall         -> ("#FFFFFF", "")

generateSVG : Array State -> Html.Html msg
generateSVG ps =
    let
        n = Array.length ps
        cells = generateSVGRow 0 ps
    in
        svg [ (viewBox ("0 0 " ++  toString (n*100) ++ " 100"))
            , Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            ] (Array.toList cells)

generateSVGRow rowIdx = Array.indexedMap
    (\k v -> g [] [ rect [ Svg.Attributes.width "100"
                         , Svg.Attributes.height "100"
                         , y <| toString <| rowIdx * 100
                         , x <| toString <| k * 100
                         , fill <| Tuple.first <| styleFn v
                         , stroke "#000000"
                         ] []
                  , Svg.text_ [ Svg.Attributes.fontSize "50px"
                              , Svg.Attributes.fontFamily "'VT323', monospace"
                              , y <| toString <| rowIdx * 100 + 60
                              , x <| toString <| (k * 100) + 25
                              ] [text <| Tuple.second <| styleFn v]
                  ]
    )
title : Html msg
title =
   Markdown.toHtml [] """

# Processor Synchronization Simulator
"""

instructions : Html msg
instructions =
   Markdown.toHtml [] """

Move the first slider to pick a k-value to simulate. The number of processors
will be 2<sup>k</sup> - 1.

Then, move the second slider to change the speed of the animation.
"""
flatten = List.foldl ((++) << Array.toList) []

tracer k =
    let
        n = 2^k - 1
        rects = flatten
            <| Tuple.first
            <| List.foldr (\i (acc, r) -> (generateSVGRow r i::acc, r+1)) ([], 0)
            <| Simulator.trace
            <| Simulator.init k
    in
        svg [ (viewBox ("0 0 " ++ toString (n*100) ++ " " ++ toString (3*n*100)))
            , Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            ] rects

toggleButton : String -> String -> a -> Bool -> Html.Html a
toggleButton trueText falseText evt v =
    button [ Html.Events.onClick evt ]
           [ text (if v then trueText else falseText) ]

view : Model -> Html Msg
view model = div []
    [ title
    , generateSVG model.current
    , instructions
    , Html.p [] [text <| "steps to go: " ++ toString model.stepsToGo]
    , Html.label [] [text <| "k-value (" ++ toString model.k ++ ")"]
    , Html.input [ Html.Attributes.defaultValue "3"
                 , Html.Attributes.type_ "range"
                 , Html.Attributes.min "3"
                 , Html.Attributes.max "10"
                 , placeholder "k-value"
                 , onInput KChange
                 ] []
    , Html.label [] [text "animation speed"]
    , Html.input [ Html.Attributes.defaultValue "1"
                 , Html.Attributes.type_ "range"
                 , Html.Attributes.min "1"
                 , Html.Attributes.max "10"
                 , placeholder "total time"
                 , onInput TimeChange
                 ] []
    , button [ Html.Events.onClick Restart ] [ text "Restart"]
    , toggleButton "Pause" "Play" TogglePlaying model.isPlaying
    , if model.k <= 7
        then toggleButton "Hide Trace" "Show Trace" ToggleTrace (model.trace /= Nothing)
        else text "[Trace Feature Unavailable]"
    , Maybe.withDefault (text "") model.trace
    ]
