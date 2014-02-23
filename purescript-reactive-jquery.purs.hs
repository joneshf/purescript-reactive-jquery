module ReactiveJQuery where

import Prelude
import Control.Monad.Eff
import JQuery
import Reactive
import Control.Monad
import Data.Array (insertAt, deleteAt, updateAt)
import Data.IORef (newIORef, readIORef, modifyIORef, unsafeRunIORef)

-- |
-- Bind the value property of a text box to the specified RVar
-- 
bindValueTwoWay ref input = do
  -- Set the value on the input to the current value
  value <- readRVar ref
  setValue value input
  
  -- Subscribe for updates on the input
  -- TODO: add this to the subscription
  flip (on "change") input $ do
    newValue <- getValue input
    writeRVar ref newValue

  -- Subscribe for updates on the RVar
  subscribe ref $ \newValue -> do
    setValue newValue input
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
bindArray arr el create = unsafeRunIORef $ do
  -- Create an IORef to hold an array of DOM elements and associated Subscriptions
  elements <- newIORef []

  -- Create a DOM element for each element currently in the array
  arr' <- readRArray arr
  flip mapM arr' $ \a -> do
    { el = child } <- create a
    child `append` el

  -- Subscribe for updates on the array
  subscribeArray arr $ \change -> case change of
    Inserted a index -> do
      element@{ el = child, subscription = sub } <- create a
      modifyIORef elements (insertAt index element)
      case index of
        0 -> append child el
        _ -> appendAtIndex index child el 
      return {}
    Updated a index -> do
      { el = old, subscription = Subscription unsubscribe } <- (flip (!!) index) <$> readIORef elements
      unsubscribe
      remove old

      element@{ el = new, subscription = sub } <- create a
      modifyIORef elements (updateAt index element)
      new `append` el
 
      return {}
    Removed index -> do
      { el = child, subscription = Subscription unsubscribe } <- (flip (!!) index) <$> readIORef elements
      unsubscribe
      remove child
      modifyIORef elements (deleteAt index 1)
      return {}

