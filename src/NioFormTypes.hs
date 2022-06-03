{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module NioFormTypes where

import Data.Text (Text)
import           Data.Proxy

data NioForm e = NioForm {
  fields :: [NioFieldView e]
  } deriving (Show, Eq)

data NioFieldError a = NioFieldErrorV a deriving Eq
--instance Eq NioFieldError where
  --(==) (NioFieldErrorV a) (NioFieldErrorV b) = show a == show b

nioerrorFailRetriveOrError :: NioFieldError String
nioerrorFailRetriveOrError = NioFieldErrorV "Failed to retrieve or return error"

deriving instance (Show e => Show (NioFieldError e))

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputTextShort
  | NioFieldInputText
  | NioFieldInputTextPassword
  | NioFieldInputLabled Bool [(Text,Text)]
  | NioFieldInputDigit
  | NioFieldInputBool String
  | NioFieldInputFile
  | NioFieldInputSubmit Text Text
  deriving (Show, Eq)

data NioFieldView e = NioFieldView
    { fvLabel :: Text
    , fvId :: Text
    , fvErrors :: [e]
    , fvType :: NioFieldInput
    , fvValue :: NioFieldVal
    } deriving Eq

data NioFieldVal = NioFieldValS String | NioFieldValM [String] deriving (Show, Eq)

deriving instance (Show e => Show (NioFieldView e))

type FieldEr e = (NioFormKey, e)
type FormInput = [(String, String)]
type NioValidateField a e = Maybe a -> Either e a
type NioFormKey = String
type NioGetField a = Either String a


class Monad m => FieldGetter m a e s where
  getField :: s -> FormInput -> m (Maybe (Either (String, [FieldEr e]) a))

class Monad m => FieldGetterErrorKey m a s where
  getFieldErrorKey :: s -> Proxy a -> m (String)

class Monad m => FieldGetterErrorMsg m e where
  renderErrors :: [FieldEr e] -> m e

instance (Monad m, Monoid e) => FieldGetterErrorMsg m e where
  renderErrors v = pure $ mconcat $ fmap snd v
