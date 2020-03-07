{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wno-orphans #-}

module NioFormInstances where

import Data.Text
import Data.String.Conversions
import NioForm
import NioFormM

instance FieldGetter Bool where
  getField "true" = Right $ True
  getField "false" = Right $ False
  getField _ = Left "NioInternal error: Invalid boolean value"

instance FieldGetter String where
  getField = Right . id

instance FieldGetter Text where
  getField = Right . cs

instance FieldGetter Int where
  getField = Right . read . cs

instance FieldGetterM IO Text where
  getFieldM x = pure $ Right $ pack x
