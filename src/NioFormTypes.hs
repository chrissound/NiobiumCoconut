{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module NioFormTypes where

import Data.Text (Text)
import           Data.Proxy

data NioForm' = NioForm' {
  fields' :: [NioFieldView]
  } deriving (Show, Eq)

data NioFieldError = forall a. (Show a, Eq a) => NioFieldErrorV a
instance Eq NioFieldError where
  (==) (NioFieldErrorV a) (NioFieldErrorV b) = show a == show b

nioerrorFailRetriveOrError :: NioFieldError
nioerrorFailRetriveOrError = NioFieldErrorV "Failed to retrieve or return error"

deriving instance Show (NioFieldError)

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputTextShort
  | NioFieldInputText
  | NioFieldInputMultiple [(Text,Text)]
  | NioFieldInputDigit
  | NioFieldInputBool (Bool)
  | NioFieldInputFile
  deriving (Show, Eq)

data NioFieldView = NioFieldView
    { fvLabel' :: Text
    , fvId' :: Text
    , fvErrors' :: [NioFieldError]
    , fvType' :: NioFieldInput
    , fvValue' :: NioFieldVal
    --, fvDetermine :: forall m a s. FieldGetter'' m a s => FormInput -> a
    } deriving Eq

data NioFieldVal = NioFieldValS String | NioFieldValM [String] deriving (Show, Eq)

deriving instance Show NioFieldView

type FieldEr = (NioFormKey, NioFieldError)
type FormInput = [(String, String)]
type NioValidateField a = Maybe (Either String a) -> NioFormKey -> Maybe (FieldEr)
type NioValidateField' a = Maybe a -> Either NioFieldError a
type NioFormKey = String
type NioGetField a = Either String a


class Monad m => FieldGetter'' m a s | a -> s where
  getField'' :: s -> FormInput -> m (Maybe (Either (String, [FieldEr]) a))
  getFieldErrorKey' :: s -> Proxy a -> m (String)
  --getFieldRaw'' :: s -> FormInput -> m (a)
  --getErrors'' :: s -> FormInput -> Proxy a -> m (Maybe (String, [FieldEr]))
