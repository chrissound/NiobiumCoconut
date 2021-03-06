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
--import Data.List

instance (Monad m) => FieldGetter m Bool String where
  getFieldErrorKey k _ = pure k
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, NioFieldErrorV $ "Failed to read value: " ++ x)])
      Nothing -> Nothing

instance (Monad m) => FieldGetter m Int String where
  getFieldErrorKey k _ = pure k
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, NioFieldErrorV $ "Failed to read value: " ++ x)])
      Nothing -> Nothing

  --getFieldRaw'' _ _ = pure $ True

--instance FieldGetter Identity a String where
  --
instance Monad m => FieldGetter m String String where
  getFieldErrorKey k _ = pure k
  getField k i = pure $ do
    case lookup k i of
      Just x -> Just $ Right x
      Nothing -> Just $ Left (k, [(k, NioFieldErrorV "??")])

--instance FieldGetter Identity String String where
  --getFieldErrorKey k _ = pure k
  --getField k i = pure $ do
    --case lookup k i of
      --Just x -> Just $ Right x
      --Nothing -> Just $ Left (k, [(k, NioFieldErrorV "??")])
instance FieldGetter Identity Text String where
  getFieldErrorKey k _ = pure k
  getField k i = pure $ do
    case lookup k i of
      Just x -> Just $ Right (cs x)
      Nothing -> Just $ Left (k, [(k, NioFieldErrorV "??")])

instance (Monad m) => FieldGetter m [String] String where
  getFieldErrorKey k _ = pure k
  getField k i = pure $ Just $ Right $ snd <$> filter ((k ==) . fst) i
