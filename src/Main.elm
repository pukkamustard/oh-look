module Main exposing (..)

import Return exposing (Return)
import Random
import Time exposing (Time)
import Char


--

import AnimationFrame
import Animation exposing (Animation)
import Ease
import Math.Vector2 as Vector2 exposing (Vec2, vec2, getX, getY)


--

import Html as H
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


--

import Keyboard


main : Program Never Model Msg
main =
    H.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Island


type alias Island =
    { position : Vec2
    }


{-| Generate a random island

TODO: check that not too close to existing islands

-}
islandGenerator : List Island -> Random.Generator Island
islandGenerator islands =
    Random.map2 (\x y -> { position = vec2 x y })
        (Random.float 0 (worldSize islands |> getX))
        (Random.float 0 (worldSize islands |> getY))



-- Post


type alias Post =
    { createdAt : Time
    , direction : Vec2
    , origin : Vec2
    }



-- Game View helpers


{-| Focus (or what is my view of the world)
-}
type Focus
    = World
    | OneIsland Island
    | Transitioning
        { to : Focus
        , viewConfig : Animation ViewConfig
        }


type alias ViewConfig =
    { size : Vec2
    , center : Vec2
    }


worldSize : List Island -> Vec2
worldSize islands =
    vec2 5000 5000


islandWorldSize : Vec2
islandWorldSize =
    vec2 500 500


viewConfig : Model -> Focus -> ViewConfig
viewConfig model focus =
    case focus of
        World ->
            { size = worldSize model.islands
            , center = worldSize model.islands |> Vector2.scale 0.5
            }

        OneIsland island ->
            { size = islandWorldSize
            , center = island.position
            }

        Transitioning { viewConfig } ->
            Animation.animate viewConfig model.time


towards : Vec2 -> Vec2 -> Float -> Vec2
towards from to c =
    Vector2.add from (Vector2.sub to from |> Vector2.scale c)


transitionFocus : Model -> Focus -> Focus -> Focus
transitionFocus model current to =
    let
        now =
            model.time

        currentViewConfig =
            viewConfig model current

        toViewConfig =
            viewConfig model to
    in
        Transitioning
            { to = to
            , viewConfig =
                (\size center -> { size = size, center = center })
                    |> Animation.pure
                    |> Animation.apply
                        (Animation.animation now
                            (1 * Time.second)
                            (Ease.inOutSine >> towards currentViewConfig.size toViewConfig.size)
                        )
                    |> Animation.apply
                        (Animation.animation now
                            (1 * Time.second)
                            (Ease.inOutSine >> towards currentViewConfig.center toViewConfig.center)
                        )
            }



-- MODEL


type alias Model =
    { time : Time
    , islands : List Island
    , focus : Focus
    }


init : Return Msg Model
init =
    { time = 0
    , islands =
        []
    , focus = World
    }
        |> Return.singleton



-- UPDATE


type Msg
    = AddIsland Island
    | SelectIsland Island
      --
    | KeyPress Keyboard.KeyCode
    | Tick Time


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        AddIsland island ->
            { model
                | islands = island :: model.islands
                , focus = transitionFocus model model.focus (OneIsland island)
            }
                |> Return.singleton

        SelectIsland island ->
            { model
                | focus = transitionFocus model model.focus (OneIsland island)
            }
                |> Return.singleton

        Tick dt ->
            { model | time = model.time + dt }
                |> Return.singleton
                |> Return.andThen updateFocus

        KeyPress keyCode ->
            if keyCode == Char.toCode 'w' then
                { model | focus = transitionFocus model model.focus World }
                    |> Return.singleton
            else if keyCode == Char.toCode 'c' then
                model
                    |> Return.singleton
                    |> Return.command (islandGenerator model.islands |> Random.generate AddIsland)
            else
                model
                    |> Return.singleton


updateFocus : Model -> Return Msg Model
updateFocus model =
    case model.focus of
        Transitioning { to, viewConfig } ->
            if Animation.isDone viewConfig model.time then
                { model | focus = to }
                    |> Return.singleton
            else
                model
                    |> Return.singleton

        _ ->
            model
                |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ AnimationFrame.diffs Tick
    , Keyboard.presses KeyPress
    ]
        |> Sub.batch



-- VIEW


{-| Helper to computer viewBox from center and size of screen
-}
viewBoxHelper : ViewConfig -> String
viewBoxHelper viewConfig =
    let
        topLeft =
            viewConfig.center
                |> Vector2.add (viewConfig.size |> Vector2.scale 0.5 |> Vector2.negate)
    in
        (topLeft |> getX |> toString)
            ++ " "
            ++ (topLeft |> getY |> toString)
            ++ " "
            ++ (viewConfig.size |> getX |> toString)
            ++ " "
            ++ (viewConfig.size |> getY |> toString)


view : Model -> H.Html Msg
view model =
    S.svg
        [ SA.width "100vw"
        , SA.height "100vh"
        , SA.display "block"
        , model.focus
            |> viewConfig model
            |> viewBoxHelper
            |> SA.viewBox
        ]
        (model.islands
            |> List.map (drawIsland model.focus)
        )


drawIsland : Focus -> Island -> S.Svg Msg
drawIsland focus island =
    S.circle
        [ SA.cx (island.position |> getX |> toString)
        , SA.cy (island.position |> getY |> toString)
        , SA.r "25"
        , SA.fill "red"
        , SE.onClick (SelectIsland island)
        ]
        []
