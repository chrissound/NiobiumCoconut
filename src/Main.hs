{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Text (Text)
import Control.Monad
import Text.Pretty.Simple (pPrint)
import Debug.Trace
import Data.Foldable

import NioForm

data TestForm = TestForm Text Text Bool Int deriving Show

main :: IO ()
main = do
  pPrint $
    --runInputForm testForm inputTest [("f1", "test"), ("f3","true"), ("f4", "4")]
    runInputForm testForm inputTest [
        ("f1", "test")
      , ("f2", "wohoo")
      , ("f3","true")
      , ("f4", "4")
    ]

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

inputTest :: FormInput -> Either ([FieldEr]) TestForm
inputTest = do
  ((liftM4 TestForm) <$> a <*> b <*> c <*> d) >>= \case
    Right x' -> pure $ pure (x')
    Left _ -> (\z -> do
                  Left $ traceShowId $ mconcat [
                       getFormErrors z [a]
                     , getFormErrors z [b]
                     , getFormErrors z [c]
                     , getFormErrors z [d]
                     ]
      )
  where
      a = myGetField (isPresent) "f1"
      b = myGetField (isPresent) "f2"
      c = myGetField (allRules[isPresent, isEq (== True) "Not true"]) "f3"
      d = myGetField (allRules[isPresent, isEq (== 4) "Not 4"]) "f4"

myGetField :: (Show a, FieldGetter a) => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
myGetField = getField (undefined)

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
