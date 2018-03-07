module Main where

import Data.Maybe
import FRP.Behavior.Keyboard
import FRP.Event
import Prelude
import PrestoDOM.Core
import PrestoDOM.Elements
import PrestoDOM.Events
import PrestoDOM.Properties
import PrestoDOM.Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import DOM (DOM)
import Data.Lens ((^.))
import Data.String (length)
import FRP (FRP)
import FRP.Behavior (Behavior, sample_)
import FRP.Event.Time (animationFrame)
import Neon.Operator ((%))
import PrestoDOM.Util (logNode, render, updateState)

type Action =
  { left :: Dynamic Boolean
  , right :: Dynamic Boolean
  , boom :: Dynamic Boolean
  }

type State =
  { x :: Int
  , y :: Int
  , xBomb :: Int
  , tar :: Boolean
  , l1 :: Int
  , l2 :: Int
  , l3 :: Int
  , l4 :: Int
  , l5 :: Int
  }


main :: forall eff. Eff ( console :: CONSOLE, frp :: FRP, dom :: DOM | eff ) Unit
main = do
    let initialState = { x : 0 , y : -50 , xBomb : 0 , tar : false , l1 : 50 , l2 : 150 , l3 : 100 , l4 : 6 , l5 : 200 }
    left <- mkDyn false
    right <- mkDyn false
    boom <- mkDyn false
    { stateBeh, updateState } <-
      render (view { left, right, boom }) initialState
    updateState
      (movement <$> (key 37) <*> (key 39) <*> (key 32) <*> stateBeh)
      (animationFrame)
      *> pure unit
  where movement left right boom oldState =
          loons left right boom
            oldState{l1=((oldState.l1+2)%550)
              ,l2=((oldState.l2+1)%550)
              ,l3=((oldState.l3+3)%550)
              ,l4=((oldState.l4+2)%550)
              ,l5=((oldState.l5+2)%550)}

loons left right boom oldState =
          fire left right boom
          (if left && oldState.x > -300
            then oldState {x = oldState.x - 10}
           else if right  && oldState.x < 300
            then oldState {x = oldState.x + 10 }
          else oldState)
fire left right boom oldState =
          incRoc left right oldState.tar
          (if boom
            then oldState{tar = true}
          else oldState)
incRoc left right boom oldState =
          crossPath left right boom
          (if boom
            then oldState {y = oldState.y-15}
          else if left && oldState.xBomb > - 300
            then oldState {xBomb = oldState.xBomb - 10}
          else if right && oldState.xBomb < 300
            then oldState {xBomb = oldState.xBomb + 10}
          else oldState)
crossPath left right boom oldState =
          if oldState.tar && oldState.y < - 600
            then oldState {xBomb = oldState.x , y = -50, tar = false}
          else oldState
view :: forall w i. Action
  -> State
  -> PrestoDOM i w
view action state =
  relativeLayout
    [ height Match_Parent
    , width Match_Parent
    , background "#323232"
    , gravity "center"
    , name "rootNode"
    ]
    [ relativeLayout
      [ height $ V 600
      , width $ V 600
      , background "gray"
      , orientation "vertical"
      , margin "320,20,20,20"
      , gravity "center"
      ][
      linearLayout[]
      [ imageView
        [ id_ "loon1"
        , height $ V 50
        , width $ V 50
        , margin ("-200,"<> show (state.l1-300)<>",0,0")
        , imageUrl "parachute"
        ]]
      ,linearLayout[][ imageView
        [ id_ "loon2"
        , height $ V 50
        , width $ V 50
        , margin ("-100,"<> show (state.l2-300)<>",0,0")
        , imageUrl "parachute"
        ]]
      ,linearLayout[][ imageView
        [ id_ "loon3"
        , height $ V 50
        , width $ V 50
        , margin ("0,"<> show (state.l3-300)<>",0,0")
        , imageUrl "parachute"
        ]]
      ,linearLayout[][ imageView
        [ id_ "loon4"
        , height $ V 50
        , width $ V 50
        , margin ("200,"<> show (state.l4-300)<>",0,0")
        , imageUrl "parachute"
        ]]
      ,linearLayout[][ imageView
        [ id_ "loon5"
        , height $ V 50
        , width $ V 50
        , margin ("100,"<> show (state.l5-300)<>",0,0")
        , imageUrl "parachute"
        ]
        ]]
      , linearLayout
        [ height $ V 150
        , width Match_Parent
        , orientation "vertical"
        , margin "20,20,20,20"
        , gravity "center"
        ]
        [linearLayout[][ imageView
          [ id_ "destroyer1"
          , height $ V 50
          , width $ V 50
          , margin (show (state.xBomb+565)<> ","<>show (state.y+600)<>",100,20")
          , imageUrl "boom"

          ]
        ]]
        ,linearLayout[][ imageView
          [ id_ "destroyer"
          , height $ V 50
          , width $ V 20
          , margin (show (state.x+600)<> ",570,100,20")
          , imageUrl "bomb"

          ]]
      ]
