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
import NioFormTypes


class FieldGetter a where
  getField :: String -> Either String a

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


fieldValue :: (Show a, FieldGetter a) =>
     NioValidateField a
  -> NioFormKey
  -> FormInput -> Either (FieldEr) a
fieldValue validate key input = do
  let val'' = case filter ((== key) . fst) input of
        (v':[]) -> pure $ snd v'
        _ -> Nothing
  let val = getField <$> val''
  case validate (mydbg'' "fieldValue val" val) key of
    Nothing -> case val of
      Just (Right x) -> Right x
      Just (Left e) -> Left $ (key, NioFieldErrorV e)
      Nothing -> Left $ (key, nioerrorFailRetriveOrError)
    Just (s, e) -> Left (s,e)



mydbg'' :: Show a => String -> a -> a
-- mydbg'' s = traceShow
--   <$> ((++) s . show)
--   <*> id
mydbg'' _ = id
