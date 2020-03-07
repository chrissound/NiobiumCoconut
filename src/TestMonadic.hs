{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# Options -Wno-orphans #-}

module TestMonadic where

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

myGetFieldIO :: (Monad m, Show a, FieldGetterM m a) =>
     NioValidateField a
  -> NioFormKey
  -> FormInput
  -> m (Either (FieldEr) a)
myGetFieldIO = fieldValueM (undefined)

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
