module Main exposing (..)

import Return exposing (Return)
import Random.Pcg as Random
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
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


--

import Uuid exposing (Uuid)
import WebSocket as WS
import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Applicative as JDA


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
    { id : Uuid
    , position : Vec2
    }


{-| Generate a random island

TODO: check that not too close to existing islands

-}
islandGenerator : List Island -> Random.Generator Island
islandGenerator islands =
    Random.map3
        (\id x y ->
            { id = id
            , position = vec2 x y
            }
        )
        Uuid.uuidGenerator
        (Random.float 0 (worldSize |> getX))
        (Random.float 0 (worldSize |> getY))


encodeIsland : Island -> JE.Value
encodeIsland island =
    JE.object
        [ ( "id", Uuid.encode island.id )
        , ( "position", encodeVec2 island.position )
        ]


islandDecoder : JD.Decoder Island
islandDecoder =
    JD.succeed Island
        |> JDA.apply (JD.field "id" Uuid.decoder)
        |> JDA.apply (JD.field "position" vec2Decoder)



-- Post


type alias Post =
    { id : Uuid
    , createdAt : Time
    , direction : Vec2
    , origin : Vec2
    , msg : String
    }


postGenerator : Time -> Vec2 -> Vec2 -> String -> Random.Generator Post
postGenerator now origin direction msg =
    Random.map
        (\id ->
            { id = id

            -- Hack to throw the bottle ahead a bit
            , createdAt = now - 5000
            , direction = direction
            , origin = origin
            , msg = msg
            }
        )
        Uuid.uuidGenerator


vec2Decoder : JD.Decoder Vec2
vec2Decoder =
    JD.succeed vec2
        |> JDA.apply (JD.field "x" JD.float)
        |> JDA.apply (JD.field "y" JD.float)


encodeVec2 : Vec2 -> JE.Value
encodeVec2 vec =
    JE.object
        [ ( "x", vec |> getX |> JE.float )
        , ( "y", vec |> getY |> JE.float )
        ]


postDecoder : JD.Decoder Post
postDecoder =
    JD.succeed Post
        |> JDA.apply (JD.field "id" Uuid.decoder)
        |> JDA.apply (JD.field "createdAt" JD.float)
        |> JDA.apply (JD.field "direction" vec2Decoder)
        |> JDA.apply (JD.field "origin" vec2Decoder)
        |> JDA.apply (JD.field "msg" JD.string)


encodePost : Post -> JE.Value
encodePost post =
    JE.object
        [ ( "id", Uuid.encode post.id )
        , ( "createdAt", JE.float post.createdAt )
        , ( "direction", encodeVec2 post.direction )
        , ( "origin", encodeVec2 post.origin )
        , ( "msg", JE.string post.msg )
        ]



-- Server communication


type ServerMsg
    = NewIsland Island
    | NewPost Post
    | Clear


serverUrl : String
serverUrl =
    "ws://localhost:9998"



--"ws://192.168.43.251:9998"


encodeServerMsg : ServerMsg -> JE.Value
encodeServerMsg msg =
    case msg of
        NewIsland island ->
            JE.object
                [ ( "type", JE.string "NewIsland" )
                , ( "island", encodeIsland island )
                ]

        NewPost post ->
            JE.object
                [ ( "type", JE.string "NewPost" )
                , ( "post", encodePost post )
                ]

        Clear ->
            JE.object
                [ ( "type", JE.string "Clear" )
                ]


serverMsgDecoder : JD.Decoder ServerMsg
serverMsgDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "NewPost" ->
                        JD.succeed NewPost
                            |> JDA.apply (JD.field "post" postDecoder)

                    "NewIsland" ->
                        JD.succeed NewIsland
                            |> JDA.apply (JD.field "island" islandDecoder)

                    _ ->
                        JD.fail "not implemented"
            )


send : ServerMsg -> Cmd msg
send msg =
    msg
        |> encodeServerMsg
        |> JE.encode 0
        |> WS.send serverUrl



-- Game View helpers


{-| Focus (or what is my view of the world)
-}
type Focus
    = World
    | Init
    | OneIsland Island
    | IslandHopping
        { to : Focus
        , viewConfig : Animation ViewConfig
        }
    | Reading
        { island : Island
        , post : Post
        }
    | Writing
        { island : Island
        , direction : Vec2
        , msg : String
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
    vec2 100 100


islandWorldSize : Vec2
islandWorldSize =
    vec2 10 10


viewConfig : Time -> Focus -> ViewConfig
viewConfig now focus =
    case focus of
        World ->
            { size = worldSize
            , center = worldSize |> Vector2.scale 0.5
            }

        Init ->
            { size = worldSize
            , center = worldSize |> Vector2.scale 0.5
            }

        OneIsland island ->
            { size = islandWorldSize
            , center = island.position
            }

        Writing { island } ->
            { size = islandWorldSize
            , center = island.position
            }

        Reading { island } ->
            { size = islandWorldSize
            , center = island.position
            }

        IslandHopping { viewConfig } ->
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
        IslandHopping
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
    , focus = Init
    }
        |> Return.singleton
        |> Return.command (Window.size |> Task.perform Resize)
        |> Return.command (Time.now |> Task.perform SetTime)
        |> Return.command (islandGenerator [] |> Random.generate CreateIsland)



-- UPDATE


type Msg
    = CreateIsland Island
    | SelectIsland Island
      --
    | UpdatePostMsg String
    | SendPost
    | CreatePost Post
      --
    | ViewPost Post
    | GoBackToIsland
      --
    | KeyPress Keyboard.KeyCode
    | Click Mouse.Position
    | Resize Window.Size
    | Tick Time
    | SetTime Time
    | CleanUp
      --
    | ServerMsg (Result String ServerMsg)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        CreateIsland island ->
            { model
                | islands = island :: model.islands
                , focus =
                    case model.focus of
                        World ->
                            transitionFocus model model.focus (OneIsland island)

                        _ ->
                            OneIsland island
            }
                |> Return.singleton
                |> Return.command (island |> NewIsland |> send)

        --|> Return.command
        --(postGenerator model.time island.position (vec2 1 0) "Hello!"
        --|> Random.generate CreatePost
        --)
        SelectIsland island ->
            { model
                | focus = transitionFocus model model.focus (OneIsland island)
            }
                |> Return.singleton

        Tick dt ->
            { model | time = model.time + dt }
                |> Return.singleton
                |> Return.andThen updateFocus

        CleanUp ->
            model
                |> Return.singleton
                |> Return.andThen dropFromTheFaceOfTheWorld

        SetTime t ->
            { model | time = t }
                |> Return.singleton

        KeyPress keyCode ->
            case model.focus of
                World ->
                    if keyCode == Char.toCode 'i' then
                        model
                            |> Return.singleton
                            |> Return.command (islandGenerator model.islands |> Random.generate CreateIsland)
                    else if keyCode == Char.toCode 'c' then
                        init
                            |> Return.command (Clear |> send)
                    else
                        model
                            |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        UpdatePostMsg msg ->
            case model.focus of
                Writing state ->
                    { model | focus = Writing { state | msg = msg } }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        SendPost ->
            case model.focus of
                Writing { island, direction, msg } ->
                    if msg == "marco polo" then
                        { model | focus = transitionFocus model model.focus World }
                            |> Return.singleton
                    else
                        { model | focus = OneIsland island }
                            |> Return.singleton
                            |> Return.command
                                (postGenerator model.time island.position direction msg
                                    |> Random.generate CreatePost
                                )

                _ ->
                    model
                        |> Return.singleton

        CreatePost post ->
            { model | posts = post :: model.posts }
                |> Return.singleton
                |> Return.command (post |> NewPost |> send)

        ViewPost post ->
            case model.focus of
                OneIsland island ->
                    { model | focus = Reading { island = island, post = post } }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        GoBackToIsland ->
            case model.focus of
                Reading { island } ->
                    { model | focus = OneIsland island }
                        |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        Click position ->
            case model.focus of
                OneIsland island ->
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

                        direction =
                            Vector2.direction worldPosition island.position
                    in
                        { model | focus = Writing { island = island, direction = direction, msg = "" } }
                            |> Return.singleton

                _ ->
                    model
                        |> Return.singleton

        Resize size ->
            { model | windowSize = size }
                |> Return.singleton

        ServerMsg (Ok (NewPost post)) ->
            { model
                | posts =
                    if List.member post model.posts then
                        model.posts
                    else
                        post :: model.posts
            }
                |> Return.singleton

        ServerMsg (Ok (NewIsland island)) ->
            { model
                | islands =
                    if List.member island model.islands then
                        model.islands
                    else
                        (island) :: model.islands
            }
                |> Return.singleton

        ServerMsg (Ok Clear) ->
            init

        ServerMsg (Err msg) ->
            let
                msg_ =
                    msg |> Debug.log "ServerMsg decoding failed"
            in
                model
                    |> Return.singleton


updateFocus : Model -> Return Msg Model
updateFocus model =
    case model.focus of
        IslandHopping { to, viewConfig } ->
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
    [ case model.focus of
        IslandHopping _ ->
            AnimationFrame.diffs Tick

        _ ->
            Time.every (250 * Time.millisecond) (always <| Tick 250)
    , Time.every (10 * Time.second) (always CleanUp)
    , Keyboard.presses KeyPress
    , case model.focus of
        OneIsland _ ->
            Mouse.clicks Click

        _ ->
            Sub.none
    , Window.resizes Resize
    , WS.listen serverUrl (JD.decodeString serverMsgDecoder >> ServerMsg)
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
    H.div []
        [ preloadAssets
        , case model.focus of
            Writing writingState ->
                writingInterface writingState

            Reading readingState ->
                readingInterface readingState

            _ ->
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
                     , case model.focus of
                        OneIsland island ->
                            model.islands
                                |> List.filter (.id >> (==) island.id)
                                |> List.map (drawIsland model.time model.focus)

                        _ ->
                            model.islands
                                |> List.map (drawIsland model.time model.focus)
                     , model.posts
                        |> List.map (drawPost model.time)
                     ]
                        |> List.concat
                    )
        ]


readingInterface : { island : Island, post : Post } -> S.Svg Msg
readingInterface { island, post } =
    let
        overlayAttributes =
            [ SA.x "0"
            , SA.y "0"
            , SA.height "9"
            , SA.width "16"
            ]

        image path content =
            S.image
                ([ SA.xlinkHref path ] ++ overlayAttributes)
                content

        waterAnimation =
            S.g []
                [ image "assets/readingInterface_water_01.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.25"
                        , SA.values "visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.id "reading-water-anim"
                        ]
                        []
                    ]
                , image "assets/readingInterface_water_02.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.25;0.5"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.begin "reading-water-anim.begin"
                        ]
                        []
                    ]
                , image "assets/readingInterface_water_03.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.5;0.75"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.begin "reading-water-anim.begin"
                        ]
                        []
                    ]
                , image "assets/readingInterface_water_04.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.75"
                        , SA.values "hidden;visible"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.begin "reading-water-anim.begin"
                        ]
                        []
                    ]
                ]
    in
        H.div
            [ HE.onClick GoBackToIsland
            ]
            [ S.svg
                [ SA.width "100%"
                , SA.height "100%"
                , SA.display "block"
                , SA.viewBox "0 0 16 9"
                ]
                [ image "assets/readingInterface_background.png" []
                , waterAnimation
                , image "assets/readingInterface_bottle.png" []
                ]
            , H.textarea
                [ HA.style
                    [ ( "position", "fixed" )
                    , ( "top", "45vh" )
                    , ( "left", "39vw" )
                    , ( "height", "45vh" )
                    , ( "width", "22vw" )
                    , ( "resize", "none" )
                    , ( "outline", "0" )
                    , ( "border", "0" )
                    , ( "font-size", "xx-large" )
                    ]
                , HA.rows 5
                , HA.cols 10
                , HA.id "msgInput"
                , HA.value post.msg
                , HA.readonly True
                ]
                []
            ]


writingInterface : { island : Island, direction : Vec2, msg : String } -> S.Svg Msg
writingInterface { island, direction, msg } =
    let
        overlayAttributes =
            [ SA.x "0"
            , SA.y "0"
            , SA.height "9"
            , SA.width "16"
            ]

        image path content =
            S.image
                ([ SA.xlinkHref path ] ++ overlayAttributes)
                content

        waterAnimation =
            S.g []
                [ image "assets/writingInterface_water_01.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.25"
                        , SA.values "visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.id "water01"
                        ]
                        []
                    ]
                , image "assets/writingInterface_water_02.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.25;0.5"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.begin "water01.begin"
                        ]
                        []
                    ]
                , image "assets/writingInterface_water_03.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.5;0.75"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.begin "water01.begin"
                        ]
                        []
                    ]
                , image "assets/writingInterface_water_04.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.75"
                        , SA.values "hidden;visible"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.begin "water01.begin"
                        ]
                        []
                    ]
                ]
    in
        H.div []
            [ S.svg
                [ SA.width "100%"
                , SA.height "100%"
                , SA.display "block"
                , SA.viewBox "0 0 16 9"
                , SE.onClick SendPost
                ]
                [ image "assets/writingInterface_noWater_Background.png" []
                , waterAnimation
                , S.image
                    ([ SA.xlinkHref "assets/send_bottle_button.png"
                     , SA.order "10"
                     , SE.onClick SendPost
                     ]
                        ++ overlayAttributes
                    )
                    []
                ]
            , H.textarea
                [ HA.style
                    [ ( "position", "fixed" )
                    , ( "top", "35vh" )
                    , ( "left", "38vw" )
                    , ( "height", "45vh" )
                    , ( "width", "22vw" )
                    , ( "resize", "none" )
                    , ( "outline", "0" )
                    , ( "border", "0" )
                    , ( "font-size", "xx-large" )
                    ]
                , HA.rows 5
                , HA.cols 10
                , HA.autofocus True
                , HA.placeholder "..."
                , HE.onInput UpdatePostMsg
                ]
                []
            ]


preloadAssets : S.Svg Msg
preloadAssets =
    let
        image path =
            S.image [ SA.xlinkHref path ]
                []
    in
        S.g [ SA.visibility "hidden" ]
            [ image "assets/island_01_base.png"
            , image "assets/island_01_01.png"
            , image "assets/island_01_02.png"
            , image "assets/island_01_03.png"
            , image "assets/island_01_02.png"
            , image "assets/island_01_underwaterGradient.svg"
            , image "assets/writingInterface_noWater_Background.png"
            , image "assets/writingInterface_water_01.png"
            , image "assets/writingInterface_water_02.png"
            , image "assets/writingInterface_water_03.png"
            , image "assets/writingInterface_water_04.png"
            ]


background : S.Svg Msg
background =
    S.image
        [ SA.xlinkHref "assets/BackGroundBlue.png"
        , SA.x "-1000"
        , SA.y "-1000"
        , SA.height "2000"
        , SA.width "2000"
        ]
        []


drawPost : Time -> Post -> S.Svg Msg
drawPost now post =
    let
        speed =
            0.0002

        position =
            post.origin
                |> Vector2.add
                    (post.direction
                        |> Vector2.scale (speed * (now - post.createdAt))
                    )

        size =
            0.5
    in
        S.g
            [ SE.onClick (ViewPost post)
            ]
            [ S.image
                [ SA.xlinkHref "assets/bottle_01.png"
                , SA.x (position |> getX |> (+) (-size / 2) |> toString)
                , SA.y (position |> getY |> (+) (-size / 2) |> toString)
                , SA.height (size |> toString)
                , SA.width (size |> toString)
                ]
                []
            ]


drawIsland : Time -> Focus -> Island -> S.Svg Msg
drawIsland now focus island =
    let
        topLeft =
            Vector2.sub
                island.position
                (islandWorldSize |> Vector2.scale 0.5)

        overlayAttributes =
            [ SA.x (topLeft |> getX |> toString)
            , SA.y (topLeft |> getY |> toString)
            , SA.height (islandWorldSize |> getX |> toString)
            , SA.width (islandWorldSize |> getY |> toString)
            ]

        image path content =
            S.image
                ([ SA.xlinkHref path ] ++ overlayAttributes)
                content

        islandAnimation =
            S.g []
                [ image "assets/island_01_base.png" []
                , image "assets/island_01_01.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.25"
                        , SA.values "visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        , SA.id "islandAnimation"
                        ]
                        []
                    ]
                , image "assets/island_01_02.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.25;0.5"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        ]
                        []
                    ]
                , image "assets/island_01_03.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.5;0.75"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        ]
                        []
                    ]
                , image "assets/island_01_04.png"
                    [ S.animate
                        [ SA.attributeName "visibility"
                        , SA.keyTimes "0;0.75;1"
                        , SA.values "hidden;visible;hidden"
                        , SA.calcMode "discrete"
                        , SA.dur "1s"
                        , SA.repeatCount "indefinite"
                        ]
                        []
                    ]
                ]
    in
        S.g
            [ case focus of
                World ->
                    SE.onClick (SelectIsland island)

                _ ->
                    SA.visibility "true"
            ]
            [ image "assets/island_01_underwaterGradient.svg" []
            , islandAnimation
            , image "assets/character_01.png" []
            , image "assets/palmTree_01_01.png" []
            ]
