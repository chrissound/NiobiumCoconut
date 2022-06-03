{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wno-orphans #-}

module NioFormInstances where

import Data.Text (Text)
import Data.String.Conversions
import NioFormTypes
import Control.Monad.Identity
import Text.Read
import MyNioFieldError
--import Data.List
--
instance (Monad m) => FieldGetterErrorKey m [String] String where
  getFieldErrorKey k _ = pure k

instance (Monad m) => FieldGetterErrorKey m Int String where
  getFieldErrorKey k _ = pure k

instance (Monad m) => FieldGetterErrorKey m Bool String where
  getFieldErrorKey k _ = pure k

instance (Monad m) => FieldGetterErrorKey m Text String where
  getFieldErrorKey k _ = pure k

instance (Monad m) => FieldGetterErrorKey m String String where
  getFieldErrorKey k _ = pure k
  
instance (Monad m) => FieldGetter m Bool MyNioFieldError String where
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, MyNioIncorrectValue k x)])
      Nothing -> Nothing

--instance (Monad m) => FieldGetterErrorKey m Int String where
  --getFieldErrorKey k _ = pure k

instance (Monad m) => FieldGetter m Int MyNioFieldError String where
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, MyNioIncorrectValue k $ "Failed to read value: " ++ x)])
      Nothing -> Nothing

--instance (Monad m) => FieldGetterErrorKey m String String where
  --getFieldErrorKey k _ = pure k

instance Monad m => FieldGetter m String MyNioFieldError String where
  getField k i = pure $ do
    case lookup k i of
      Just x -> Just $ Right x
      Nothing -> Just $ Left (k, [(k, MyNioFieldErrorNotPresent)])

instance Monad m => FieldGetter m [String] MyNioFieldError String where
  getField = getFieldList

--instance (Monad m) => FieldGetterErrorKey m Text String where
  --getFieldErrorKey k _ = pure k

instance FieldGetter Identity Text MyNioFieldError String where
  getField k i = pure $ do
    case lookup k i of
      Just x -> Just $ Right (cs x)
      Nothing -> Just $ Left (k, [(k, MyNioFieldErrorNotPresent)])

--instance FieldGetter Identity String MyNioFieldError String where
  --getField k i = pure $ do
    --case lookup k i of
      --Just x -> Just $ Right (cs x)
      --Nothing -> Just $ Left (k, [(k, MyNioFieldErrorNotPresent)])


--instance (Monad m) => FieldGetterErrorKey m [String] String where
  --getFieldErrorKey k _ = pure k

--instance (Monad m) => FieldGetter m [String] e String where
  --getField k i = pure $ Just $ Right $ snd <$> filter ((k ==) . fst) i
