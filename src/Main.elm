module Main exposing (..)

import Return exposing (Return)
import Random
import Time exposing (Time)
import Char
import Task


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

import Window
import Keyboard
import Mouse


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
        (Random.float 0 (worldSize |> getX))
        (Random.float 0 (worldSize |> getY))



-- Post


type alias Post =
    { createdAt : Time
    , direction : Vec2
    , origin : Vec2
    , msg : String
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


topLeft : ViewConfig -> Vec2
topLeft viewConfig =
    viewConfig.center
        |> Vector2.add (viewConfig.size |> Vector2.scale 0.5 |> Vector2.negate)


worldSize : Vec2
worldSize =
    vec2 10000 10000


islandWorldSize : Vec2
islandWorldSize =
    vec2 500 500


viewConfig : Time -> Focus -> ViewConfig
viewConfig now focus =
    case focus of
        World ->
            { size = worldSize
            , center = worldSize |> Vector2.scale 0.5
            }

        OneIsland island ->
            { size = islandWorldSize
            , center = island.position
            }

        Transitioning { viewConfig } ->
            Animation.animate viewConfig now


towards : Vec2 -> Vec2 -> Float -> Vec2
towards from to c =
    Vector2.add from (Vector2.sub to from |> Vector2.scale c)


transitionFocus : Model -> Focus -> Focus -> Focus
transitionFocus model current to =
    let
        now =
            model.time

        currentViewConfig =
            viewConfig model.time current

        toViewConfig =
            viewConfig model.time to
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
    , windowSize : Window.Size

    --
    , islands : List Island
    , posts : List Post
    , focus : Focus
    }


init : Return Msg Model
init =
    { time = 0
    , windowSize = { width = 100, height = 100 }

    --
    , islands = []
    , posts = []
    , focus = World
    }
        |> Return.singleton
        |> Return.command (Window.size |> Task.perform Resize)
        |> Return.command (Time.now |> Task.perform SetTime)
        |> Return.command (islandGenerator [] |> Random.generate AddIsland)



-- UPDATE


type Msg
    = AddIsland Island
    | SelectIsland Island
      --
    | KeyPress Keyboard.KeyCode
    | Click Mouse.Position
    | Resize Window.Size
    | Tick Time
    | SetTime Time


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
                |> Return.andThen dropFromTheFaceOfTheWorld

        SetTime t ->
            { model | time = t }
                |> Return.singleton

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

        Click position ->
            let
                relative =
                    { x = (toFloat position.x / toFloat model.windowSize.width)
                    , y = (toFloat position.y / toFloat model.windowSize.height)
                    }

                worldPosition =
                    Vector2.add
                        (viewConfig model.time model.focus |> topLeft)
                        (vec2
                            (viewConfig model.time model.focus |> .size |> getX |> (*) relative.x)
                            (viewConfig model.time model.focus |> .size |> getY |> (*) relative.y)
                        )
                        |> Debug.log "Click"
            in
                { model
                    | posts =
                        case model.focus of
                            OneIsland island ->
                                { createdAt = model.time
                                , origin = island.position
                                , direction = Vector2.direction worldPosition island.position
                                , msg = "Hello!"
                                }
                                    :: model.posts

                            _ ->
                                model.posts
                }
                    |> Return.singleton

        Resize size ->
            { model | windowSize = size }
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


dropFromTheFaceOfTheWorld : Model -> Return Msg Model
dropFromTheFaceOfTheWorld model =
    { model
        | posts =
            model.posts
                |> List.filter (\post -> model.time - post.createdAt <= 2 * Time.minute)
    }
        |> Return.singleton



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ AnimationFrame.diffs Tick
    , Keyboard.presses KeyPress
    , Mouse.clicks Click
    , Window.resizes Resize
    ]
        |> Sub.batch



-- VIEW


{-| Helper to computer viewBox from center and size of screen
-}
viewBoxHelper : ViewConfig -> String
viewBoxHelper viewConfig =
    (viewConfig |> topLeft |> getX |> toString)
        ++ " "
        ++ (viewConfig |> topLeft |> getY |> toString)
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
            |> viewConfig model.time
            |> viewBoxHelper
            |> SA.viewBox
        ]
        ([ [ background ]
         , model.islands
            |> List.map (drawIsland model.focus)
         , model.posts
            |> List.map (drawPost model.time)
         ]
            |> List.concat
        )


background : S.Svg Msg
background =
    S.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.height (worldSize |> getY |> toString)
        , SA.width (worldSize |> getX |> toString)
        , SA.fill "aqua"
        , SA.opacity "0.5"
        ]
        []


drawPost : Time -> Post -> S.Svg Msg
drawPost now post =
    let
        speed =
            0.05

        position =
            post.origin
                |> Vector2.add
                    (post.direction
                        |> Vector2.scale (speed * (now - post.createdAt))
                    )
    in
        S.g []
            [ S.circle
                [ SA.cx (position |> getX |> toString)
                , SA.cy (position |> getY |> toString)
                , SA.r "10"
                , SA.stroke "Blue"
                , SA.fill "none"
                ]
                []
            , S.text_
                [ SA.x (position |> getX |> toString)
                , SA.y (position |> getY |> toString)
                ]
                [ S.text post.msg ]
            ]


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
