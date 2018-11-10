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
  getField "true" = True
  getField _ = False

instance FieldGetter String where
  getField = id

instance FieldGetter Text where
  getField = cs

instance FieldGetter Int where
  getField = read . cs
