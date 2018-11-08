{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NioForm where

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.String.Conversions
import Debug.Trace

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving Show

data NioFieldError =
    NioFieldErrorEmty Text
  | NioIncorrectValue Text
  | NioFieldInternalFailure deriving Show

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputText
  | NioFieldInputMultiple [(Text,Text)]
  | NioFieldInputDigit
  deriving Show

data NioFieldView = forall a. Show a => NioFieldView
    { fvLabel :: Text
    , fvId :: Text
    , fvErrors :: [NioFieldError]
    , fvType :: NioFieldInput
    , fvValue :: a
    }

deriving instance Show NioFieldView

class FieldGetter a where
  getField' :: String ->  a

instance FieldGetter Bool where
  getField' "true" = True
  getField' _ = False

instance FieldGetter Text where
  getField' = cs

instance FieldGetter Int where
  getField' _ = 123

data MyEither a b = MyLeft a | MyRight b
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

hydrateErrors :: (Eq a, ConvertibleStrings Text a) =>
    [(a, NioFieldError)]
  ->NioFieldView
  ->NioFieldView
hydrateErrors e nfv = nfv {
  fvErrors = (snd) <$> (filter (\(s,_) -> s == cs (fvId nfv)) e)
  }

getFormErrors :: FormInput -> [FormInput -> Either (FieldEr) a] -> [FieldEr]
getFormErrors input functions = catMaybes $ (
  \x -> case x input of
    Right _ -> Nothing
    Left e -> Just e
  ) <$> functions

getField :: (Show a, FieldGetter a) => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
getField validate key input = do
  let val = case filter ((== key) . fst) input of
        (v':[]) -> pure $ getField' $ snd v'
        [] -> Nothing
        _ -> error "wtf???"
  case validate (traceShowId val) key of
    Nothing -> case val of
      Just x -> Right x
      Nothing -> Left $ (key, NioFieldInternalFailure)
    Just (s, e) -> Left (s,e)
