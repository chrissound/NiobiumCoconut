{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NioForm where

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.String.Conversions
import Debug.Trace

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving Show

data NioFieldError = forall a. (Show a) => NioFieldErrorV a
deriving instance Show (NioFieldError)

data MyNioFieldError =
    MyNioFieldErrorEmty 
  | MyNioIncorrectValue Text
  | MyNioFieldInternalFailure deriving Show

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
  getField' :: String ->  a

instance FieldGetter Bool where
  getField' "true" = True
  getField' _ = False

instance FieldGetter Text where
  getField' = cs

instance FieldGetter Int where
  getField' = read . cs

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

hydrateErrors :: forall a. (Eq a, ConvertibleStrings Text a, Show a) =>
    [(a, NioFieldError)]
  ->NioFieldView
  ->NioFieldView
hydrateErrors e nf = nf {
    fvErrors = (snd) <$> (filter ((==) (cs (fvId nf)) . fst) e)
    --fvErrors = (fmap snd e)
  }

getFormErrors :: FormInput -> [FormInput -> Either (FieldEr) a] -> [FieldEr]
getFormErrors input functions = catMaybes $ (
  \x -> case x input of
    Right _ -> Nothing
    Left e -> Just e
  ) <$> functions

getField :: (Show a, FieldGetter a) => NioFieldError -> (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
getField b' validate key input = do
  let val = case filter ((== key) . fst) input of
        (v':[]) -> pure $ getField' $ snd v'
        [] -> Nothing
        _ -> error "wtf???"
  case validate (traceTraceShowId ("getField for key: " ++ key) val) key of
    Nothing -> case val of
      Just x -> Right x
      Nothing -> Left $ (key, b')
    Just (s, e) -> Left (s,e)

traceTraceShowId :: Show a => String -> a -> a
traceTraceShowId x =  trace "" . trace "Func:" . trace x . trace "--" . traceShowId
