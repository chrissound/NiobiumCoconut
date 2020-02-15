{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either

import NioForm

data TestForm = TestForm Text Text Bool Int deriving (Show, Eq)
data TestForm2 = TestForm2 Text Text deriving (Show, Eq)

data MyNioFieldError =
    MyNioFieldErrorEmty
  | MyNioIncorrectValue Text
  | MyNioFieldInternalFailure deriving (Show, Eq)

myTests :: [TestTree]
myTests = [
      testCase "initial" $ do
          x <-  runInputFormM testForm inputTest [("f1", "test"), ("f3","true"), ("f4", "4")]
          (x @?= Right (TestForm2 "Test" "Abc"))
    , testCase "failure" $ do
        let x = runInputForm testForm inputTestP' [("f1", "test"), ("f3","true"), ("f4", "4")]
        (True @?= (isLeft x))
    , testCase "more success" $ do
        let x = runInputForm testForm inputTestP' [("f1", "test"),("f2", "test"), ("f3","true"), ("f4", "4")]
        (True @?= (isRight x))
    , testCase "more success2 " $ do
        x <- runInputFormM testForm inputTestP [
            ("f1", "test")
          , ("f2", "wohoo")
          , ("f3","true")
          , ("f4", "4")
          ]
        (True @?= (isRight x))
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
main = defaultMain $ testGroup "Tests:" myTests

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) $ \x -> do
  print x

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

inputTest :: forall. FormInput -> IO (Either ([FieldEr]) TestForm2)
inputTest = do
      ((liftM2 TestForm2) <$> a <*> b) >>= \case
        Right x' -> pure . pure $ Right x' -- const 5 -- undefined --pure $ pure (x')
        Left e -> id (e  :: FormInput -> IO (Either ([FieldEr]) TestForm2))
  where
      a _ = (pure "Test")

        -- myGetFieldIO
        -- (pure "Test")  -- :: FormInput -> Either (FormInput -> IO (Either [FieldEr] TestForm2)) Text
      b = do
        const $ pure "Abc"
-- inputTest :: FormInput -> IO (Either ([FieldEr]) TestForm)
-- inputTest = do
--           (fmap pure)
--       <$> (liftM4 TestForm)
--       <$> a <*> b <*> c <*> d
--       >>= \case
--         Right x' -> pure $ pure (x')
--         Left _ -> undefined
--       -- Left _ -> (\z -> do
--       --               Left $ traceShowId $ mconcat [
--       --                    getFormErrors z [a]
--       --                  , getFormErrors z [b]
--       --                  , getFormErrors z [c]
--       --                  , getFormErrors z [d]
--       --                  ]
--       --   )
--   where
--       a = myGetFieldIO (isPresent) "f1"                         :: FormInput -> IO (Either FieldEr a)
--       b = pure . myGetField (isPresent) "f2"                    :: FormInput -> IO (Either FieldEr a)
--       c = pure . myGetField
--           (allRules[isPresent, isEq (== True) "Not true"]) "f3" :: FormInput -> IO (Either FieldEr a)
--       d = pure . myGetField
--           (allRules[isPresent, isEq (== 4) "Not 4"]) "f4"       :: FormInput -> IO (Either FieldEr a)

myGetField :: (Show a, FieldGetter a) => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
myGetField = fieldValue (undefined)

myGetFieldIO :: (Monad m, Show a, FieldGetterM m a) => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> m (Either (FieldEr) a)
myGetFieldIO = fieldValueM (undefined)

isPresent :: Maybe b -> a -> Maybe (a, NioFieldError)
isPresent x k = case x of
  Just _ -> Nothing
  Nothing -> Just (k, NioFieldErrorV $ MyNioFieldErrorEmty)

isEq :: (b -> Bool) -> Text -> Maybe b -> a -> Maybe (a, NioFieldError)
isEq f t x k = case x of
  Just x' -> if f x' then Nothing else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  _ -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Not true")

allRules ::  [Maybe b -> a -> Maybe (a, NioFieldError)] -> Maybe b -> a -> Maybe (a, NioFieldError)
allRules r v k = asum $ fmap (\r' -> r' v k) r

inputTestP :: forall. FormInput -> IO (Either ([FieldEr]) TestForm2)
inputTestP fi = do
    allErrors <- collect fi
    (first $ const $ allErrors) <$>
      ((
        ((liftM2 . liftM2) TestForm2) <$> a <*> b
      ) fi)
  where
      collect z = mconcat [
          getFormErrorsM z [a]
        , getFormErrorsM z [b]
        ]
      a = (pure) <$> myGetField (isPresent) "f1" :: FormInput -> IO (Either FieldEr Text)
      b = myGetFieldIO (isPresent) "f2" :: FormInput -> IO (Either FieldEr Text)

instance FieldGetterM IO Text where
  getFieldM x = pure (pack  $ (x ++)  " www")

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

      a = myGetField (isPresent) "f1"
      b = myGetField (isPresent) "f2"
      c = myGetField
          (allRules[isPresent, isEq (== True) "Not true"]) "f3"
      d = myGetField
          (allRules[isPresent, isEq (== 4) "Not 4"]) "f4"
