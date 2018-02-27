module PrestoDOM.Util where

import PrestoDOM.Core

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Node.Types (Element, Document)
import FRP (FRP)
import FRP as F
import FRP.Behavior (Behavior, behavior, sample_)
import FRP.Behavior as B
import FRP.Event (Event, subscribe)
import FRP.Event as E
import Halogen.VDom (Step(..), VDom, VDomMachine, VDomSpec(..), buildVDom, extract)
import Halogen.VDom.Machine (never, step, extract)
import Prelude (Unit, Void, bind, const, discard, pure, unit, ($))
import PrestoDOM.Properties (a_duration)
import PrestoDOM.Types (Rec)

foreign import logNode :: forall eff a . a  -> Eff eff Unit
foreign import applyAttributes ∷ forall i eff. Element → (Array (Prop i)) → Eff eff (Array (Prop i))
foreign import done :: forall eff. Eff eff Unit
foreign import patchAttributes ∷ forall i eff. Element → (Array (Prop i)) → (Array (Prop i)) → Eff eff (Array (Prop i))
foreign import cleanupAttributes ∷ forall i eff. Element → (Array (Prop i)) → Eff eff Unit
foreign import getLatestMachine :: forall m a b eff. Eff eff (Step m a b)
foreign import storeMachine :: forall eff m a b. Step m a b -> Eff eff Unit
foreign import getRootNode :: forall eff. Eff eff Document
foreign import insertDom :: forall a b eff. a -> b -> Eff eff Unit
-- foreign import attachSignalEvents :: forall a b eff.  String -> String -> (b ->  Eff (frp::F.FRP | eff) Unit) -> Unit
-- foreign import initializeState :: forall eff t . Eff eff Unit
-- foreign import updateState :: forall eff a b t. a  -> b -> Eff eff (Rec t)
-- foreign import getState :: forall eff t. Eff eff (Rec t)

buildAttributes
  ∷ ∀ eff a
  . Element
  → VDomMachine eff (Array (Prop a)) Unit
buildAttributes elem = apply
  where
  apply ∷ forall e. VDomMachine e (Array (Prop a)) Unit
  apply attrs = do
    x <- applyAttributes elem attrs
    pure
      (Step unit
        (patch x)
        (done x))

  patch ∷ forall e. (Array (Prop a)) → VDomMachine e (Array (Prop a)) Unit
  patch attrs1 attrs2 = do
    x <- patchAttributes elem attrs1 attrs2
    pure
      (Step unit
        (patch x)
        (done x))

  done ∷ forall e. (Array (Prop a)) → Eff e Unit
  done attrs = cleanupAttributes elem attrs

spec :: forall i e. Document -> VDomSpec e (Array (Prop i)) Void
spec document =  VDomSpec {
      buildWidget: const never
    , buildAttributes: buildAttributes
    , document : document
    }

render :: forall i state eff a.
  (state -> VDom (Array (Prop i)) Void)
  -> state
  -> Eff ( dom :: DOM, frp :: FRP | eff )
    {  updateState :: (Behavior state
                      -> Event a
                      -> Eff (frp :: FRP, dom :: DOM | eff) (Eff (frp :: FRP, dom :: DOM | eff) Unit))
    , stateBeh :: Behavior state }
render dom state = do
  root <- getRootNode
  machine <- buildVDom (spec root) (dom state)
  storeMachine machine
  insertDom root (extract machine)
  { event, push } <- E.create
  let stateBeh = B.step state event
  _ <- sample_ stateBeh event `E.subscribe` (\x -> patchAndRun x dom)
  pure { updateState : (updateState push), stateBeh }

updateState :: forall eff state a. (state -> Eff ( frp :: FRP | eff ) Unit)
  -> Behavior state
  -> Event a
  -> Eff (frp :: FRP | eff) (Eff (frp :: FRP | eff) Unit)
updateState stateFn beh event = sample_ beh event `subscribe` (\newState -> do
  _ <- logNode newState
  stateFn newState)


patchAndRun :: forall a b t state i. state -> (state -> VDom (Array (Prop i)) Void) -> Eff t Unit
patchAndRun state myDom = do
  machine <- getLatestMachine
  newMachine <- step machine (myDom state)
  storeMachine newMachine


-- patchAndRun :: forall a b t. Eff ( console :: CONSOLE | t ) a -> (a -> b) -> Eff ( console :: CONSOLE | t ) Unit
-- patchAndRun x myDom = do
--   state <- x
--   log "patching"
--   machine <- getLatestMachine
--   newMachine <- step machine (myDom state)
--   storeMachine newMachine

-- render :: forall i t a. VDom (Array (Prop i)) Void -> Eff ( dom :: DOM | t ) a -> Eff ( dom :: DOM | t ) Unit
-- render dom listen = do
--   root <- getRootNode
--   machine <- buildVDom (spec root) dom
--   storeMachine machine
--   insertDom root (extract machine)
--   _ <- listen
--   pure unit


-- signal :: forall t a. String -> String -> a -> Eff ( frp :: F.FRP | t ) { behavior :: B.Behavior a , event :: E.Event a }
-- signal name eventType initialValue = do
--   o <- E.create
--   let behavior = B.step initialValue o.event
--   let x = attachSignalEvents name eventType o.push
--   pure $ {behavior : behavior , event : o.event}

-- patch :: forall e a b t. (b -> a) -> B.Behavior (Eff ( console :: CONSOLE , frp :: F.FRP | t ) b ) -> E.Event e -> Eff ( frp :: F.FRP , console :: CONSOLE | t ) (Eff ( frp :: F.FRP , console :: CONSOLE | t ) Unit )
-- patch dom behavior events = do
--   B.sample_ behavior events `E.subscribe` (\x -> patchAndRun x dom)