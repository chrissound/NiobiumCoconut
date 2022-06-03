{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# Options -Wno-orphans #-}
{-# Options -Wno-unused-matches #-}
{-# Options -Wno-simplifiable-class-constraints#-}

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
import Control.Applicative
import MyNioFieldError
import Control.Monad.Identity

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

renderNioForm :: NioForm MyNioFieldError -> IO ()
renderNioForm nf = forM_ (fields nf) print

emptyError :: [MyNioFieldError] 
emptyError = []

testForm :: NioForm MyNioFieldError
testForm = NioForm
  [ NioFieldView "Test"   "f1" emptyError NioFieldInputText (NioFieldValS "")
  , NioFieldView "Test 2" "f2" emptyError NioFieldInputText (NioFieldValS "")
  , NioFieldView "Test 3"
                 "f3"
                 emptyError
                 (NioFieldInputLabled False [("a", "a"), ("b", "b")])
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
                --
                --
                --
                --
--fieldValueS :: a -> String -> c
fieldValueS :: forall a e .
                 (FieldGetter Identity a e String,
                  FieldGetterErrorKey Identity a String,
                  FieldGetterErrorMsg Identity e) =>
                 NioValidateField a e -> String -> FormInput -> Either (FieldEr e) a
fieldValueS x s = fieldValue x s

fieldValueS' :: forall e .
                 (FieldGetter Identity [String] e String,
                  FieldGetterErrorKey Identity [String] String,
                  FieldGetterErrorMsg Identity e) =>
                 NioValidateField [String] e -> String -> FormInput -> Either (FieldEr e) [String]
fieldValueS' x s = fieldValue x s

inputTestP' :: FormInput -> Either ([FieldEr MyNioFieldError]) TestForm
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

  a = fieldValueS (isPresent) "f1" :: FormInput -> Either (FieldEr MyNioFieldError) Text
  b = fieldValueS (isPresent) "f2"
  c = fieldValueS (isEq (== True) "Not true") "f3"
  d = fieldValueS (isEq (== 4) "Not 4") "f4"
  e = fieldValueS' (isPresent) "f5" :: FormInput -> Either (FieldEr MyNioFieldError) [String]

inputTestP'' :: FormInput -> Either ([FieldEr MyNioFieldError]) TestForm
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

  a = fieldValueS (isPresent) "f1" :: FormInput -> Either (FieldEr MyNioFieldError) Text
  b = fieldValueS (isPresent) "f2"
  c = fieldValueS (isEq (== True) "Not true") "f3"
  d = fieldValueS (isEq (== 4) "Not 4") "f4"
  e = fieldValueS (isEq ((>=3) . (Data.Foldable.length :: [String] -> Int)) "Not more than 2 entries") "f5"

yolo :: IO (Int, Int)
yolo = do
  (liftA2 (,) <$> a <*> b) 20 where
  a _ = pure 10 :: IO Int
  b _ = pure 10 :: IO Int

yoloinputTestP''' :: FormInput -> IO (Either MyNioFieldError TestForm)
yoloinputTestP''' fi = 
  ((liftM5 . liftM5) TestForm <$> a <*> b <*> c <*> d <*> e) fi
 where
  a  = const (pure $ pure "") :: FormInput -> IO (Either MyNioFieldError Text)
  b _ = pure $ pure "" :: IO (Either MyNioFieldError Text)
  c _ = pure $ pure True :: IO (Either MyNioFieldError Bool)
  d _ = pure $ pure 10 :: IO (Either MyNioFieldError Int)
  e _ = pure $ pure [] :: IO (Either MyNioFieldError [String])

yoloinputTestP'''' :: FormInput -> IO (Either [MyNioFieldError] TestForm)
yoloinputTestP'''' fi = do
  allErrors' <- mconcat <$> sequence allErrors
  (first $ const allErrors') <$> ((liftM5 . liftM5) TestForm <$> a <*> b <*> c <*> d <*> e) fi
 where
  allErrors =
    [ getFormErrorsM fi [a]
    , getFormErrorsM fi [b]
    ]
  a  = const(pure $ pure "") :: FormInput -> IO (Either MyNioFieldError Text)
  b _ = pure $ pure "" :: IO (Either MyNioFieldError Text)
  c _ = pure $ pure True :: IO (Either MyNioFieldError Bool)
  d _ = pure $ pure 10 :: IO (Either MyNioFieldError Int)
  e _ = pure $ pure [] :: IO (Either MyNioFieldError [String])

--inputTestP''' :: FormInput -> IO (Either (FieldEr) TestForm)
--inputTestP''' fi = 
  --(((liftM5 . liftM5) TestForm) <$> a <*> b <*> c <*> d <*> e) fi
 --where
  --a _ = fieldValue' undefined undefined undefined :: IO (Either FieldEr Text)
  --b _ = fieldValue' undefined undefined undefined :: IO (Either FieldEr Text)
  --c _ = fieldValue' undefined undefined undefined :: IO (Either FieldEr Bool)
  --d _ = fieldValue' undefined undefined undefined :: IO (Either FieldEr Int)
  --e _ = fieldValue' undefined undefined undefined :: IO (Either FieldEr [String])
