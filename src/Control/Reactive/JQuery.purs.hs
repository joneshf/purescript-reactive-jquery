module Control.Reactive.JQuery where

import Prelude (($), (<$>), (-), (>), (+), flip, return)
import Prelude.Unsafe (unsafeIndex)
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Eff.Ref (newRef, readRef, modifyRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.JQuery
import Control.Reactive
import Data.Array (insertAt, deleteAt, updateAt, range, length, drop)
import Data.Either
import Data.Foreign
import Data.Traversable

-- |
-- Bind the value property of an input field to the specified RVar
-- i
bindValueTwoWay :: forall a eff. (ReadForeign a) => RVar a -> JQuery -> Eff (reactive :: Reactive, dom :: DOM | eff) Subscription
bindValueTwoWay ref input = do
  -- Set the value on the input to the current value
  value <- readRVar ref
  setValue value input

  -- Subscribe for updates on the input
  -- TODO: add this to the subscription
  flip (on "change") input $ \_ -> do
    Right newValue <- parseForeign read <$> getValue input
    writeRVar ref newValue

  -- Subscribe for updates on the RVar
  subscribe ref $ \newValue -> do
    setValue newValue input
    return {}

-- |
-- Bind the checked property of a checkbox to the specified RVar
--
bindCheckedTwoWay :: forall eff. RVar Boolean -> JQuery -> Eff (reactive :: Reactive, dom :: DOM | eff) Subscription
bindCheckedTwoWay ref checkbox = do
  -- Set the checked status based on the current value
  value <- readRVar ref
  setProp "checked" value checkbox

  -- Subscribe for updates on the checkbox
  -- TODO: add this to the subscription
  flip (on "change") checkbox $ \_ -> do
    Right newValue <- parseForeign read <$> getProp "checked" checkbox
    writeRVar ref newValue

  -- Subscribe for updates on the RVar
  subscribe ref $ \newValue -> do
    setProp "checked" newValue checkbox
    return {}

-- |
-- Bind the text content of an element to the specified computed value
--
bindTextOneWay comp el = do
  -- Set the text on the element to the current value
  text <- readComputed comp
  setText text el

  -- Subscribe for updates on the computed value
  subscribeComputed comp $ \text -> do
    setText text el
    return {}

-- |
-- Bind an RArray
--
bindArray :: forall a eff. RArray a -> JQuery ->
                           (a -> RVar Number -> Eff eff { el :: JQuery, subscription :: Subscription }) ->
                           Eff (reactive :: Reactive, dom :: DOM | eff) Subscription
bindArray arr el create = unsafeRunRef $ do
  -- Create a DOM element for each element currently in the array
  arr' <- readRArray arr
  elements' <- zipWithA (\a index -> do
    indexR <- newRVar index
    { el = child, subscription = subscription } <- unsafeInterleaveEff $ create a indexR
    child `append` el
    return { el: child
           , subscription: subscription
           , index: indexR }) arr' (range 0 (length arr' - 1))
  elements <- newRef elements'

  -- Subscribe for updates on the array
  subscribeArray arr $ \change -> case change of
    Inserted a index -> do
      indexR <- newRVar index
      { el = child, subscription = subscription } <- unsafeInterleaveEff $ create a indexR
      others <- readRef elements
      traverse (\{ index = indexR } -> modifyRVar indexR (\i -> if i > index then i + 1 else i)) others
      modifyRef elements $
        insertAt index { el: child
                       , subscription: subscription
                       , index: indexR }
      appendAtIndex index child el
      return {}
    Updated a index -> do
      { el = old, subscription = Subscription unsubscribe, index = indexR } <- flip unsafeIndex index <$> readRef elements
      unsubscribe
      remove old

      { el = new, subscription = subscription } <- unsafeInterleaveEff $ create a indexR
      modifyRef elements $
        updateAt index { el: new
                       , subscription: subscription
                       , index: indexR }
      appendAtIndex index new el

      return {}
    Removed index -> do
      { el = child, subscription = Subscription unsubscribe } <- flip unsafeIndex index <$> readRef elements
      unsubscribe
      remove child
      modifyRef elements (deleteAt index 1)
      others <- readRef elements
      traverse (\{ index = indexR } -> modifyRVar indexR (\i -> if i > index then i - 1 else i)) others
      return {}

