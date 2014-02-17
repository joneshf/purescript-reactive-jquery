module ReactiveJQuery where

import Prelude
import Eff
import JQuery
import Reactive
import Monad

-- |
-- Bind the value property of a text box to the specified RVar
-- 
bindValueTwoWay ref input = do
  -- Set the value on the input to the current value
  value <- readRVar ref
  setValue value input
  
  -- Subscribe for updates on the input
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
bindArray arr el create = do
  -- Create a DOM element for each element currently in the array
  arr' <- readRArray arr
  flip mapM arr' $ \a -> do
    child <- create a
    child `append` el

  -- Subscribe for updates on the array
  subscribeArray arr $ \change -> case change of
    Inserted a index -> do
      child <- create a
      -- TODO: insert the child at the right index
      child `append` el 
      return {}
    Updated a index -> do
      child <- create a
      -- TODO: remove the existing DOM element
      child `append` el 
      -- TODO: remove the existing bindings
      return {}
    Removed index -> do
      -- TODO: remove the bindings
      return {}

