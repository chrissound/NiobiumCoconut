{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Text (Text)
import Data.Bool 
import Control.Monad
import Data.Maybe (catMaybes)
import Data.String.Conversions

import NioForm

main :: IO ()
main = do
  renderNioForm testForm
  print ""
  print $ runInputForm testForm []

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) $ \x -> do
  print x

testForm :: NioForm
testForm = NioForm [
    NioFieldView "Test" "test" []   NioFieldInputText "test"
  , NioFieldView "Test2" "test2" [] NioFieldInputText "test"
  , NioFieldView "Test3" "test3" []
    (NioFieldInputMultiple [("a","a"), ("b","b")])
    "test"
  , NioFieldView "Test4" "test4" [] NioFieldInputDigit 10
  ]
