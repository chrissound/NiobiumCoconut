{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# Options -Wno-orphans #-}
{-# Options -Wno-unused-matches #-}

module Test where

import           Data.Text                      ( Text )
import           Control.Monad
-- import Text.Pretty.Simple (pPrint)
import           Debug.Trace
import           Data.Foldable
import           NioFormInstances
import           Data.Bifunctor
import           Data.Text
import           Data.String.Conversions
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Either

import           NioForm
import           NioFormTypes

import           TestXyz
import           Text.Pretty.Simple

myTests :: [TestTree]
myTests =
  [
      --testCase "initial" $ do
          --x <-  runInputFormM testForm inputTest [("f1", "Test"), ("f2","Abc")]
          --(x @?= Right (TestForm2 "Test" "Abc"))
    --,
    testCase "failure" $ do
    let x = runInputForm testForm
                         inputTestP'
                         [("f1", "test"), ("f3", "True"), ("f4", "4")]
    (True @?= (isLeft x))
  , testCase "more success" $ do
    let x = runInputForm
          testForm
          inputTestP'
          [ ("f1", "test")
          , ("f2", "test")
          , ("f3", "True")
          , ("f4", "4")
          , ("f5", "abc")
          , ("f5", "xyz")
          ]
    pPrint x
    let xt = Right $ TestForm  "test" "test" True 4 ["abc","xyz"]
    (x @?= xt)
  , testCase "more success" $ do
    let x = runInputForm
          testForm
          inputTestP''
          [ ("f1", "test")
          , ("f2", "test")
          , ("f3", "True")
          , ("f4", "4")
          , ("f5", "abc")
          , ("f5", "xyz")
          ]
    pPrint x
    (True @?= (isLeft x))
    --testCase "more success2 " $ do
         --x <- runInputFormM testForm inputTestP [
             --("f1", "test")
           --, ("f2", "wohoo")
           --, ("f3","true")
           --, ("f4", "4")
           --]
         --(True @?= (isRight x))
                         -- fields = [
                         --       NioFieldView {
                         --           fvLabel = "Test"
                         --         , fvId = "f1"
                         --         , fvErrors = []
                         --         , fvType = NioFieldInputText
                         --         , fvValue = "test"}
                         --     , NioFieldView {
                         --           fvLabel = "Test 2"
                         --         , fvId = "f2"
                         --         , fvErrors = [NioFieldErrorV MyNioFieldErrorEmty]
                         --         , fvType = NioFieldInputText
                         --         , fvValue = ""}
                         --     , NioFieldView {
                         --           fvLabel = "Test 3"
                         --         , fvId = "f3"
                         --         , fvErrors = []
                         --         , fvType = NioFieldInputMultiple [("a"
                         --         ,"a")
                         --         ,("b"
                         --         ,"b")]
                         --         , fvValue = "true"}
                         --     , NioFieldView {
                         --           fvLabel = "Test4"
                         --         , fvId = "f4"
                         --         , fvErrors = []
                         --         , fvType = NioFieldInputDigit
                         --         , fvValue = "4"}]
                         -- }))
  ]

main :: IO ()
main = do
  print "hi"
  defaultMain $ testGroup "Tests:" myTests
  print "done"

renderNioForm :: NioForm' -> IO ()
renderNioForm nf = forM_ (fields' nf) print

emptyError :: [NioFieldError]
emptyError = []

testForm :: NioForm'
testForm = NioForm'
  [ NioFieldView "Test"   "f1" emptyError NioFieldInputText (NioFieldValS "")
  , NioFieldView "Test 2" "f2" emptyError NioFieldInputText (NioFieldValS "")
  , NioFieldView "Test 3"
                 "f3"
                 emptyError
                 (NioFieldInputMultiple [("a", "a"), ("b", "b")])
                 (NioFieldValS $ show True)
  , NioFieldView "Test4"
                 "f4"
                 emptyError
                 NioFieldInputDigit
                 (NioFieldValS $ show 0)
  , NioFieldView "Test5"
                 "f5"
                 emptyError
                 NioFieldInputDigit
                 (NioFieldValM ["hello", "world"])
  ]

--inputTest :: FormInput -> IO (Either [FieldEr] TestForm2)
--inputTest fi = do
    --allErrors' <- (mconcat <$> sequence (allErrors :: [IO [FieldEr]]))
    --(first $ const allErrors')
      -- <$> ((liftM2 . liftM2) TestForm2 <$> a <*> b) fi
  --where
    --a = (pure <$> myGetField (isPresent) "f1") :: FormInput -> IO (Either FieldEr Text)
    --b = myGetFieldIO (isPresent) "f2"
    --allErrors = [
        --getFormErrorsM fi [a]
      --, getFormErrorsM fi [b]
                --]

inputTestP' :: FormInput -> Either ([FieldEr]) TestForm
inputTestP' fi = (first $ const $ collect fi)
  (((liftM5 TestForm) <$> a <*> b <*> c <*> d <*> e) fi)
 where
  collect z = mconcat
    [ getFormErrors z [a]
    , getFormErrors z [b]
    , getFormErrors z [c]
    , getFormErrors z [d]
    , getFormErrors z [e]
    ]

  a = fieldValue (isPresent) "f1" :: FormInput -> Either FieldEr Text
  b = fieldValue (isPresent) "f2"
  c = fieldValue (isEq (== True) "Not true") "f3"
  d = fieldValue (isEq (== 4) "Not 4") "f4"
  e = fieldValue (isPresent) "f5"

inputTestP'' :: FormInput -> Either ([FieldEr]) TestForm
inputTestP'' fi = (first $ const $ collect fi)
  (((liftM5 TestForm) <$> a <*> b <*> c <*> d <*> e) fi)
 where
  collect z = mconcat
    [ getFormErrors z [a]
    , getFormErrors z [b]
    , getFormErrors z [c]
    , getFormErrors z [d]
    , getFormErrors z [e]
    ]

  a = fieldValue (isPresent) "f1" :: FormInput -> Either FieldEr Text
  b = fieldValue (isPresent) "f2"
  c = fieldValue (isEq (== True) "Not true") "f3"
  d = fieldValue (isEq (== 4) "Not 4") "f4"
  e = fieldValue (isEq ((>=3) . Data.Foldable.length) "Not more than 2 entries") "f5"
