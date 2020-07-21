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
import NioForm
import NioFormTypes
import Control.Monad.Identity
import Text.Read
--import Data.List

instance FieldGetter Bool where
  getField "true" = Right $ True
  getField "false" = Right $ False
  getField _ = Left "NioInternal error: Invalid boolean value"

instance FieldGetter String where
  getField = Right . id


instance Read a => FieldGetter'' Identity a String where
  getFieldErrorKey' k _ = pure k
  getField'' k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, NioFieldErrorV $ "Failed to read value: " ++ x)])
      Nothing -> Nothing

  --getFieldRaw'' _ _ = pure $ True

--instance FieldGetter'' Identity a String where
instance {-# OVERLAPPING #-} FieldGetter'' Identity String String where
  getFieldErrorKey' k _ = pure k
  getField'' k i = pure $ do
    case lookup k i of
      Just x -> Just $ Right x
      Nothing -> Just $ Left (k, [(k, NioFieldErrorV "??")])
instance {-# OVERLAPPING #-} FieldGetter'' Identity Text String where
  getFieldErrorKey' k _ = pure k
  getField'' k i = pure $ do
    case lookup k i of
      Just x -> Just $ Right (cs x)
      Nothing -> Just $ Left (k, [(k, NioFieldErrorV "??")])
instance {-# OVERLAPPING #-} FieldGetter'' Identity [String] String where
  getFieldErrorKey' k _ = pure k
  getField'' k i = pure $ Just $ Right $ snd <$> filter ((k ==) . fst) i
--instance FieldGetter'' Identity Text String where
  --getField'' _ _ = pure $ pure ""
  ----getFieldRaw'' _ _ = pure $ ""
  --getErrors'' _ _ _ = pure $ Nothing
--instance FieldGetter'' Identity Bool String where
  --getField'' k i = do
    --case lookup k i of
      --Just "true" -> pure $ Right True
      --Just "false" -> pure $ Right False
      --Just _ -> pure $ Left (k, [(k, NioFieldErrorV "??")])
      --Nothing -> pure $ Left (k, [(k, NioFieldErrorV "??")])

  ----getFieldRaw'' _ _ = pure $ True
  --getErrors'' _ _ _ = pure $ Nothing
--instance FieldGetter'' Identity Int String where
  --getField'' _ _ = pure $ pure 10
  --getFieldRaw'' _ _ = pure $ 10
  --getErrors'' _ _ _ = pure $ Nothing
--instance FieldGetter a => FieldGetter'' Identity a b where

--instance FieldGetter [String] where
  --getField x = x

instance FieldGetter Text where
  getField = Right . cs

instance FieldGetter Int where
  getField = Right . read . cs
