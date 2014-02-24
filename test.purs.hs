module Main where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Data.Monoid
import Debug.Trace
import Data.Array (map, foldl)
import Global (parseInt)
import JQuery
import Reactive
import ReactiveJQuery

main = do
  personDemo
  todoListDemo

greet firstName lastName = "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

personDemo = do
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

todoListDemo = do
  -- Get the document body
  b <- body

  -- Create an array
  arr <- newRArray
  
  ul <- create "<ul>"

  -- Bind the ul to the array
  bindArray arr ul $ \entry index -> do
    li <- create "<li>"

    countInput <- create "<input>"
    countInput `append` li
    sub1 <- bindValueTwoWay entry.count countInput
    
    textInput <- create "<input>"
    textInput `append` li
    sub2 <- bindValueTwoWay entry.text textInput

    btn <- create "<button>"
    "Remove" `appendText` btn
    flip (on "click") btn $ do
      removeRArray arr index
    btn `append` li

    return { el: li, subscription: sub1 <> sub2 }

  ul `append` b

  -- Add button
  newEntryDiv <- create "<div>"
  btn <- create "<button>"
  "Add" `appendText` btn
  btn `append` newEntryDiv
  newEntryDiv `append` b

  flip (on "click") btn $ do
    text <- newRVar ""
    count <- newRVar "1"
    insertRArray arr { text: text, count: count } 0

  -- Create a paragraph to display the task counter
  counterLabel <- create "<p>"
  counterLabel `append` b

  let counter = (flip (++) " tasks remaining") <<< show <$> do
    rs <- toComputedArray arr
    cs <- map parseInt <$> mapM (\entry -> toComputed entry.count) rs
    return $ foldl (+) 0 cs
  bindTextOneWay counter counterLabel
