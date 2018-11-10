{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NioForm where

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.String.Conversions

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving Show

data NioFieldError = forall a. (Show a) => NioFieldErrorV a
deriving instance Show (NioFieldError)

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputText
  | NioFieldInputMultiple [(Text,Text)]
  | NioFieldInputDigit
  deriving Show

data NioFieldView = NioFieldView
    { fvLabel :: Text
    , fvId :: Text
    , fvErrors :: [NioFieldError]
    , fvType :: NioFieldInput
    , fvValue :: String
    }

deriving instance Show NioFieldView

class FieldGetter a where
  getField :: String ->  a

type FieldEr = (String, NioFieldError)
type FormInput = [(String, String)]

runInputForm :: 
     NioForm
  -> (FormInput -> Either [FieldEr] a)
  -> FormInput
  -> Either NioForm a
runInputForm nf = fmap (\case
  Right x -> Right x
  Left e -> Left $ NioForm { fields = fmap (hydrateErrors e) (fields nf)}
  )

hydrateErrors :: forall a. (Eq a, ConvertibleStrings Text a, Show a) =>
    [(a, NioFieldError)]
  ->NioFieldView
  ->NioFieldView
hydrateErrors e nf = nf {
    fvErrors = snd <$> filter ((==) (cs (fvId nf)) . fst) e
  }

getFormErrors :: FormInput -> [FormInput -> Either (FieldEr) a] -> [FieldEr]
getFormErrors input = catMaybes .  fmap (
  \x -> case x input of
    Right _ -> Nothing
    Left e -> Just e
  )

fieldValue :: (Show a, FieldGetter a) => NioFieldError -> (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
fieldValue b' validate key input = do
  let val = case filter ((== key) . fst) input of
        (v':[]) -> pure $ getField $ snd v'
        _ -> Nothing
  case validate val key of
    Nothing -> case val of
      Just x -> Right x
      Nothing -> Left $ (key, b')
    Just (s, e) -> Left (s,e)
