module Main where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Data.Monoid
import Debug.Trace
import JQuery
import Reactive
import ReactiveJQuery

greet firstName lastName = "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

main = do
  -- Create new reactive variables to hold the user's names
  firstName <- newRVar "John"
  lastName <- newRVar "Smith"

  -- Get the document body
  b <- body

  -- Create a text box for the first name
  firstNameDiv <- create "<div>"
  firstNameInput <- create "<input>"
  "First Name: " `appendText` firstNameDiv
  firstNameInput `append` firstNameDiv
  firstNameDiv `append` b

  -- Create a text box for the last name
  lastNameDiv <- create "<div>"
  lastNameInput <- create "<input>"
  "Last Name: " `appendText` lastNameDiv
  lastNameInput `append` lastNameDiv
  lastNameDiv `append` b

  -- Bind the text box values to the name variables
  bindValueTwoWay firstName firstNameInput
  bindValueTwoWay lastName lastNameInput

  -- Create a paragraph to display a greeting
  greeting <- create "<p>"
  { color: "red" } `css` greeting
  greeting `append` b

  -- Bind the text property of the greeting paragraph to a computed property
  let greetingC = greet <$> toComputed firstName <*> toComputed lastName
  bindTextOneWay greetingC greeting




  -- Create an array
  arr <- newRArray
  
  ul <- create "<ul>"

  -- Bind the ul to the array
  bindArray arr ul $ \entry -> do
    li <- create "<li>"
    entryInput <- create "<input>"
    entryInput `append` li
    sub <- bindValueTwoWay entry entryInput
    readRVar entry >>= flip setValue entryInput

    btn <- create "<button>"
    "Remove" `appendText` btn
    flip (on "click") btn $ do
      removeRArray arr 0
    btn `append` li

    return { el: li, subscription: sub }

  ul `append` b

  -- Add button
  newEntryDiv <- create "<div>"
  btn <- create "<button>"
  "Add" `appendText` btn
  btn `append` newEntryDiv
  newEntryDiv `append` b

  flip (on "click") btn $ do
    r <- newRVar ""
    insertRArray arr r 0

  -- Create a paragraph to display a concatenated string
  allTextLabel <- create "<p>"
  allTextLabel `append` b

  let allText = do
    rs <- toComputedArray arr
    ss <- mapM toComputed rs
    return $ mconcat ss
  bindTextOneWay allText allTextLabel


concatAll :: forall eff. [RVar String] -> Eff (reactive :: Reactive | eff) String
concatAll rs = do
  ss <- mapM readRVar rs
  return $ mconcat ss
