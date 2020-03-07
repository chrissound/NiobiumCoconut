{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# Options -Wno-orphans #-}

module Test where

import Data.Text (Text)
import Control.Monad
-- import Text.Pretty.Simple (pPrint)
import Debug.Trace
import Data.Foldable
import NioFormInstances
import Data.Bifunctor
import Data.Text
import Data.String.Conversions
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either

import NioForm
import NioFormM
import NioFormTypes

import TestXyz
import TestMonadic


myTests :: [TestTree]
myTests = [
      testCase "initial" $ do
          x <-  runInputFormM testForm inputTest [("f1", "Test"), ("f2","Abc")]
          (x @?= Right (TestForm2 "Test" "Abc"))
    , testCase "failure" $ do
        let x = runInputForm testForm inputTestP' [("f1", "test"), ("f3","true"), ("f4", "4")]
        (True @?= (isLeft x))
    , testCase "more success" $ do
        let x = runInputForm testForm inputTestP' [("f1", "test"),("f2", "test"), ("f3","true"), ("f4", "4")]
        print x
        (True @?= (isRight x))
    -- , testCase "more success2 " $ do
    --     x <- runInputFormM testForm inputTestP [
    --         ("f1", "test")
    --       , ("f2", "wohoo")
    --       , ("f3","true")
    --       , ("f4", "4")
    --       ]
    --     (True @?= (isRight x))
    --                     -- fields = [
    --                     --       NioFieldView {
    --                     --           fvLabel = "Test"
    --                     --         , fvId = "f1"
    --                     --         , fvErrors = []
    --                     --         , fvType = NioFieldInputText
    --                     --         , fvValue = "test"}
    --                     --     , NioFieldView {
    --                     --           fvLabel = "Test 2"
    --                     --         , fvId = "f2"
    --                     --         , fvErrors = [NioFieldErrorV MyNioFieldErrorEmty]
    --                     --         , fvType = NioFieldInputText
    --                     --         , fvValue = ""}
    --                     --     , NioFieldView {
    --                     --           fvLabel = "Test 3"
    --                     --         , fvId = "f3"
    --                     --         , fvErrors = []
    --                     --         , fvType = NioFieldInputMultiple [("a"
    --                     --         ,"a")
    --                     --         ,("b"
    --                     --         ,"b")]
    --                     --         , fvValue = "true"}
    --                     --     , NioFieldView {
    --                     --           fvLabel = "Test4"
    --                     --         , fvId = "f4"
    --                     --         , fvErrors = []
    --                     --         , fvType = NioFieldInputDigit
    --                     --         , fvValue = "4"}]
    --                     -- }))
        ]

main :: IO ()
main = defaultMain $ testGroup "Tests:" myTests

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) print

emptyError :: [NioFieldError]
emptyError = []

testForm :: NioForm
testForm = NioForm [
       NioFieldView "Test" "f1" emptyError   NioFieldInputText ""
     , NioFieldView "Test 2" "f2" emptyError NioFieldInputText ""
     , NioFieldView "Test 3" "f3" emptyError
        (NioFieldInputMultiple [("a","a"), ("b","b")])
        (show True)
     , NioFieldView "Test4" "f4" emptyError NioFieldInputDigit (show 0)
  ]

inputTest :: FormInput -> IO (Either [FieldEr] TestForm2)
inputTest fi = do
    allErrors' <- (mconcat <$> sequence (allErrors :: [IO [FieldEr]]))
    (first $ const allErrors')
      <$> ((liftM2 . liftM2) TestForm2 <$> a <*> b) fi
  where
    a = (pure <$> myGetField (isPresent) "f1") :: FormInput -> IO (Either FieldEr Text)
    b = myGetFieldIO (isPresent) "f2"
    allErrors = [
        getFormErrorsM fi [a]
      , getFormErrorsM fi [b]
                ]

inputTestP' :: FormInput -> Either ([FieldEr]) TestForm
inputTestP' fi =
  (first $ const $ collect fi)
    (((liftM4 TestForm) <$> a <*> b <*> c <*> d) fi)
  where
      collect z = mconcat [
                          getFormErrors z [a]
                        , getFormErrors z [b]
                        , getFormErrors z [c]
                        , getFormErrors z [d]
                        ]

      a = myGetField (isPresent) "f1" :: FormInput -> Either FieldEr Text
      b = myGetField (isPresent) "f2"
      c = myGetField (isEq (== True) "Not true") "f3"
      d = myGetField (isEq (== 4) "Not 4") "f4"

