module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import DOM (DOM)
import Data.Lens ((^.))
import Data.String (length)
import FRP (FRP)
import FRP.Behavior (Behavior, sample_)
import FRP.Event
import FRP.Event.Keyboard (down)
import FRP.Behavior.Keyboard
import Data.Maybe

import PrestoDOM.Core
import PrestoDOM.Elements
import PrestoDOM.Events
import PrestoDOM.Properties
import PrestoDOM.Types
import PrestoDOM.Util (logNode, render, updateState)

type Action =
  { left :: Dynamic Boolean
  , right :: Dynamic Boolean
  , boom :: Dynamic Boolean
  }

type State =
  { x :: Int
}


main :: forall eff. Eff ( console :: CONSOLE, frp :: FRP, dom :: DOM | eff ) Unit
main = do
    let initialState = { x : 0 }
    left <- mkDyn false
    right <- mkDyn false
    boom <- mkDyn false
    { stateBeh, updateState } <-
      render (view { left, right, boom }) initialState
    updateState
      (movement <$> (key 37) <*> (key 39) <*> (key 32) <*> stateBeh)
      (down)
      *> pure unit
  where movement left right boom oldState = if left && oldState.x > -300 then oldState {x = oldState.x - 10} else if right  && oldState.x < 300 then oldState {x = oldState.x + 10} else oldState

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
      ]
      [ imageView
        [ id_ "loon1"
        , height $ V 50
        , width $ V 50
        , margin "100,20,20,20"
        , imageUrl "parachute"
        ]
      , imageView
        [ id_ "loon2"
        , height $ V 50
        , width $ V 50
        , margin "200,0,20,20"
        , imageUrl "parachute"
        ]
      , imageView
        [ id_ "loon3"
        , height $ V 50
        , width $ V 50
        , margin "300,100,20,20"
        , imageUrl "parachute"
        ]
      , imageView
        [ id_ "loon4"
        , height $ V 50
        , width $ V 50
        , margin "0,200,20,20"
        , imageUrl "parachute"
        ]
      , imageView
        [ id_ "loon5"
        , height $ V 50
        , width $ V 50
        , margin "100,300,20,20"
        , imageUrl "parachute"
        ]
        ]
      , linearLayout
        [ height $ V 150
        , width Match_Parent
        , orientation "vertical"
        , margin "20,20,20,20"
        , gravity "center"
        ]
        [ imageView
          [ id_ "destroyer"
          , height $ V 50
          , width $ V 20
          , margin (show state.x<> ",550,100,20")
          , imageUrl "bomb"

          ]
        ]
      ]
