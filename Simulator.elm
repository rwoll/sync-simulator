module Simulator exposing (..)

import Array exposing (Array, indexedMap, get)

type Direction
    = TowardsLeft
    | TowardsRight

type State
    = Null
    | Go
    | Wait1 Direction
    | Wait2 Direction
    | Ping Direction
    | SlowPing Direction
    | ReturnPing Direction
    | Middle
    | PreTada
    | Tada
    | Wall

transition : (State, State, State) -> State
transition (left, current, right) =
    case (left, current, right) of
        -- Tada's stick around and Middles turn to PreTada after one clock cycle
        (_, Tada  , _) -> Tada
        (_, Middle, _) -> PreTada

        -- Initiate
        (_, Go    , _) -> Ping TowardsRight

        -- If we are next to a wall (where PreTada's) count as walls, countdown
        -- to send off SlowPing
        (Wall   , Ping TowardsRight, _      ) -> Wait2 TowardsRight
        (PreTada, Ping TowardsRight, _      ) -> Wait2 TowardsRight
        (_      , Ping TowardsLeft , Wall   ) -> Wait2 TowardsLeft
        (_      , Ping TowardsLeft , PreTada) -> Wait2 TowardsLeft

        -- We're ready iff you and your neighbors are done; let's do this!
        (PreTada, PreTada, PreTada) -> Tada
        (Wall   , PreTada, PreTada) -> Tada
        (PreTada, PreTada, Wall   ) -> Tada

        -- PreTada's stick around (unless we are at the end as indicated by the
        -- above patterns)
        (_, PreTada, _) -> PreTada

        -- Neighbors are standing by, so let's standby with them!
        (PreTada, _, PreTada) -> PreTada
        (Wall,    _, PreTada) -> PreTada
        (PreTada, _, Wall   ) -> PreTada

        -- HACK: Compiler was having issues with the pattern matching
        --       optimization, so we broke this out (arbitrarily)
        --       losing some compiler niceness...oh well!
        _ -> transition2 (left, current, right)

transition2 : (State, State, State) -> State
transition2 (left, current, right) =
    case (left, current, right) of
        -- We're about to intersect => Middle
        (SlowPing   _, _, ReturnPing _) -> Middle
        (ReturnPing _, _, SlowPing   _) -> Middle

        -- Kickoff "subproblem"
        (_     , _, Middle) -> Ping TowardsLeft
        (Middle, _, _     ) -> Ping TowardsRight

        -- Pass on standard Pings (and ReturnPings) 1 processor per cycle
        (Ping TowardsRight, _, _               ) -> Ping TowardsRight
        ( _               , _, Ping TowardsLeft) -> Ping TowardsLeft

        (ReturnPing TowardsRight, _, _                     ) -> ReturnPing TowardsRight
        (_                      , _, ReturnPing TowardsLeft) -> ReturnPing TowardsLeft

        -- Observe SlowPing being passed to you, and countdown to pass it on
        (SlowPing TowardsRight, _, _                   ) -> Wait2 TowardsRight
        (_                    , _, SlowPing TowardsLeft) -> Wait2 TowardsLeft

        -- Finish the countdown
        (_, Wait2 d, _) -> Wait1 d
        (_, Wait1 d, _) -> SlowPing d

        -- Bounce Pings off Walls or PreTadas
        (_      , Ping TowardsRight, Wall   ) -> ReturnPing TowardsLeft
        (_      , Ping TowardsRight, PreTada) -> ReturnPing TowardsLeft
        (Wall   , Ping TowardsLeft , _      ) -> ReturnPing TowardsRight
        (PreTada, Ping TowardsLeft , _      ) -> ReturnPing TowardsRight

        -- Catchall (Remove to debug which patterns are missing.)
        _ -> Null

step : Array State -> Array State
step ps =
    let
        defaultGet i =  Maybe.withDefault Wall <| get i ps
        transitionAux i e = transition (defaultGet <| i - 1, e, defaultGet <| i + 1)
    in
        indexedMap transitionAux ps

init : Int -> Array State
init k = Array.initialize (2^k - 1) (\i -> if i == 0 then Go else Null)

numSteps : Array a -> Int
numSteps ps =
    let
        n = Array.length ps |> toFloat
    in
        ceiling <| 3 * n - (logBase 2 n) + 1

trace : Array State -> List (Array State)
trace ps = loopN (numSteps ps) step [ps]

loopN : Int -> (a -> a) -> List a -> List a
loopN n fn lst =
    if n <= 0 then
        lst
    else
        case List.head lst of
            Just x  -> loopN (n - 1) fn <| fn x::lst
            Nothing -> lst