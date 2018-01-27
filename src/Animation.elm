module Animation
    exposing
        ( Animation
        , animate
        , animateFlip
          --
        , animation
        , static
        , age
          --
        , shift
          --
        , isDone
          --
        , map
          --
        , pure
        , apply
        )

{-| An abstracton for working with time-varying values.

Inspired by:

  - <https://hackage.haskell.org/package/active-0.2.0.12/docs/Data-Active.html>
  - <https://ku-fpg.github.io/files/Matlage-10-BeginningMiddleEnd.pdf>
  - <https://github.com/mgold/elm-animation/>

-}

import Time exposing (Time)


{-| An animation.
-}
type Animation a
    = Animated Time Time (Float -> a)
    | Static a


{-| Interpret an Animation as a function from time.
-}
animate : Animation a -> Time -> a
animate animation t =
    case animation of
        Animated start end f ->
            if t < start then
                f 0
            else if t > end then
                f 1
            else
                f ((t - start) / (end - start))

        Static a ->
            a


animateFlip : Time -> Animation a -> a
animateFlip =
    flip animate



-- Create


animation : Time -> Time -> (Float -> a) -> Animation a
animation start duration f =
    Animated start (start + duration) f


static : a -> Animation a
static a =
    Static a


age : Time -> Time -> Animation Float
age start end =
    Animated start end identity



-- Modify


shift : Time -> Animation a -> Animation a
shift t animation =
    case animation of
        Animated start end f ->
            Animated (start + t) (end + t) f

        Static v ->
            animation



-- Inspect


{-| Determine if an animation is done, meaning that it has arrived at its final value. Static animations are always done.
-}
isDone : Animation a -> Time -> Bool
isDone animation time =
    case animation of
        Animated start end _ ->
            end < time

        Static _ ->
            True



-- Functor


map : (a -> b) -> Animation a -> Animation b
map f animation =
    case animation of
        Animated start end g ->
            Animated start end (f << g)

        Static x ->
            Static (f x)



-- Applicative


pure : a -> Animation a
pure x =
    Static x


apply : Animation a -> Animation (a -> b) -> Animation b
apply a f =
    case ( f, a ) of
        ( Static f, x ) ->
            map f x

        ( Animated start end f, Static x ) ->
            Animated start end (\t -> (f t) x)

        ( Animated start0 end0 f, Animated start1 end1 g ) ->
            Animated (min start0 start1) (max end0 end1) (\t -> (f t (g t)))
