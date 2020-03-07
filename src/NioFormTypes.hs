{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module NioFormTypes where

import Data.Text (Text)
import Data.Function

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving (Show, Eq)

data NioFieldError = forall a. (Show a, Eq a) => NioFieldErrorV a
instance Eq NioFieldError where
  (==) = on (==) NioFieldErrorV

deriving instance Show (NioFieldError)

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputTextShort
  | NioFieldInputText
  | NioFieldInputMultiple [(Text,Text)]
  | NioFieldInputDigit
  | NioFieldInputFile
  deriving (Show, Eq)

data NioFieldView = NioFieldView
    { fvLabel :: Text
    , fvId :: Text
    , fvErrors :: [NioFieldError]
    , fvType :: NioFieldInput
    , fvValue :: String
    } deriving Eq

deriving instance Show NioFieldView

type FieldEr = (NioFormKey, NioFieldError)
type FormInput = [(String, String)]
type NioValidateField a = Maybe (Either String a) -> NioFormKey -> Maybe (FieldEr)
type NioFormKey = String
type NioGetField a = Either String a
