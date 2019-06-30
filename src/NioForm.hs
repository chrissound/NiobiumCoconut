{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NioForm where

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.String.Conversions
-- import Debug.Trace

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving Show

data NioFieldError = forall a. (Show a) => NioFieldErrorV a
deriving instance Show (NioFieldError)

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputTextShort
  | NioFieldInputText
  | NioFieldInputMultiple [(Text,Text)]
  | NioFieldInputDigit
  | NioFieldInputFile
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

class (Monad m) => FieldGetterM m a where
  getFieldM :: String -> m a

type FieldEr = (String, NioFieldError)
type FormInput = [(String, String)]

runInputForm ::
     NioForm
  -> (FormInput -> Either [FieldEr] a)
  -> FormInput
  -> Either NioForm a
runInputForm nf fieldValidators formInput = case fieldValidators formInput of
  Right x -> Right x
  Left e -> Left $ NioForm {
          fields = fmap (hydrateValues formInput . hydrateErrors e) (fields nf)
        }

runInputFormM :: (Monad m) =>
     NioForm
  -> (FormInput -> m (Either [FieldEr] a))
  -> FormInput
  -> m (Either NioForm a)
runInputFormM nf fieldValidators formInput = fieldValidators formInput >>= \case
  Right x -> pure $ Right x
  Left e -> pure $ Left $ NioForm {
          fields = fmap (hydrateValues formInput . hydrateErrors e) (fields nf)
        }

hydrateValues :: FormInput -> NioFieldView -> NioFieldView
hydrateValues fi nf = nf {
    fvValue = do
        case filter ((== (fvId nf)) . cs . fst) fi of
          (v':_) -> snd v'
          _ -> fvValue nf
  }

hydrateErrors :: forall a. (Eq a, ConvertibleStrings Text a, Show a) =>
    [(a, NioFieldError)]
  ->NioFieldView
  ->NioFieldView
hydrateErrors e nf = nf {
      fvErrors = snd <$> filter ((==) (cs (fvId nf)) . fst) e
  }

getFormErrors :: t -> [t -> Either a b] -> [a]
getFormErrors input = catMaybes .  fmap (
  \x -> case x input of
    Right _ -> Nothing
    Left e -> Just e
  )

getFormErrorsM :: Monad m => v -> [v -> m (Either a b)] -> m [a]
getFormErrorsM fv l = do
  vars <- sequence $ (\x -> x fv) <$> l
  pure . catMaybes $  (\x -> case x of Right _ -> Nothing; Left e -> Just e) <$> vars


fieldValue :: (Show a, FieldGetter a) => NioFieldError -> (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
fieldValue b' validate key input = do
  let val'' = case filter ((== key) . fst) input of
        (v':[]) -> pure $ snd v'
        _ -> Nothing
  let val = (getField) <$> val''
  case validate (mydbg'' "fieldValue val" val) key of
    Nothing -> case val of
      Just x -> Right x
      Nothing -> Left $ (key, b')
    Just (s, e) -> Left (s,e)

runMaybeM :: Monad m => Maybe (m a) -> m (Maybe a)
runMaybeM = \case
  (Just mia) -> Just <$> mia
  Nothing -> pure Nothing

fieldValueM :: forall m a . (Monad m, Show a, FieldGetterM m a) =>
  NioFieldError ->
  (Maybe a -> String -> Maybe (FieldEr)) ->
  String ->
  FormInput ->
  m (Either (FieldEr) a)
fieldValueM b' validate key input = do
  let val'' = case filter ((== key) . fst) input of
        (v':[]) -> pure $ snd v'
        _ -> Nothing
  val <- runMaybeM (getFieldM <$> val'')
  case (validate (val :: Maybe (a)) key) of
    Nothing -> case val of
      Just x -> pure $ Right x
      Nothing -> pure $ Left $ (key, b')
    Just (s, e) -> pure $ Left (s,e)


mydbg'' :: Show a => String -> a -> a
-- mydbg'' s = traceShow
--   <$> ((++) s . show)
--   <*> id
mydbg'' _ = id
