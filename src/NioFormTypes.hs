{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module NioFormTypes where

import Data.Text (Text)
import           Data.Proxy
import Control.Applicative
--import Data.Monoid
--import Control.Monad

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving (Show, Eq)

data NioFieldError = forall a. (Show a, Eq a) => NioFieldErrorV a
instance Eq NioFieldError where
  (==) (NioFieldErrorV a) (NioFieldErrorV b) = show a == show b

instance Alternative (Either NioFieldError) where
    (<|>) a b = case (a, b) of
                  (Right x, Right _) -> Right x
                  (_, Right y) -> Right y
                  (Right x, _) -> Right x
                  (_, _) -> empty
    empty = Left $ NioFieldErrorV "alternativ empty"

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
    { fvLable :: Text
    , fvId :: Text
    , fvErrors :: [NioFieldError]
    , fvType :: NioFieldInput
    , fvValue :: NioFieldVal
    --, fvDetermine :: forall m a s. FieldGetter m a s => FormInput -> a
    } deriving Eq

data NioFieldVal = NioFieldValS String | NioFieldValM [String] deriving (Show, Eq)

deriving instance Show NioFieldView

type FieldEr = (NioFormKey, NioFieldError)
type FormInput = [(String, String)]
type NioValidateField a = Maybe a -> Either NioFieldError a
type NioFormKey = String
type NioGetField a = Either String a


class Monad m => FieldGetter m a s | a -> s where
  getField :: s -> FormInput -> m (Maybe (Either (String, [FieldEr]) a))
  getFieldErrorKey :: s -> Proxy a -> m (String)
