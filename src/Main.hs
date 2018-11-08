{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Text (Text)
import Data.Bool
import Control.Monad
import Text.Pretty.Simple (pPrint)
import Debug.Trace
import Data.Foldable

import NioForm

data TestForm = TestForm Text Text Bool Int deriving Show

main :: IO ()
main = do
  -- renderNioForm testForm
  -- print ""
  pPrint $ runInputForm testForm inputTest [("f1", "test"), ("f3","aoeu")]

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) $ \x -> do
  print x

testForm :: NioForm
testForm = NioForm [
    NioFieldView "Test" "f1" []   NioFieldInputText ""
  , NioFieldView "Test 2" "f2" [] NioFieldInputText ""
  , NioFieldView "Test 3" "f3" []
    (NioFieldInputMultiple [("a","a"), ("b","b")])
    ""
  , NioFieldView "Test4" "f4" [] NioFieldInputDigit 0
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
      a = getField (isPresent) "f1"
      b = getField (isPresent) "f2"
      c = getField (allRules [isPresent, isEq (== True) "True"]) "f3"
      d = getField (\x k -> x >>= (errr k (NioFieldErrorEmty "not test") ) . ((==) 5)) "f4"
      errr key e = traceShowId . bool (Just (key, e)) Nothing

isPresent :: Maybe b -> a -> Maybe (a, NioFieldError)
isPresent x k = case x of
  Just _ -> Nothing
  Nothing -> Just (k, NioFieldErrorEmty "")

isEq :: (b -> Bool) -> Text -> Maybe b -> a -> Maybe (a, NioFieldError)
isEq f t x k = case x of
  Just x' -> if f x' then Nothing else Just (k, NioIncorrectValue $ "Value is not: " <> t)
  _ -> Just (k, NioIncorrectValue "Not true")

allRules ::  [Maybe b -> a -> Maybe (a, NioFieldError)] -> Maybe b -> a -> Maybe (a, NioFieldError)
allRules r v k = asum $ fmap (\r' -> r' v k) r
