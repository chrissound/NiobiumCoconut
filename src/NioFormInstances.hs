{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}

module NioFormInstances where

import Data.Text (Text)
import Data.String.Conversions
import NioForm

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
