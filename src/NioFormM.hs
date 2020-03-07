{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NioFormM where

import NioForm
import Types

class (Monad m) => FieldGetterM m a where
  getFieldM :: String -> m (NioGetField a)

runMaybeM :: Monad m => Maybe (m (NioGetField a)) -> m (Maybe (NioGetField a))
runMaybeM = sequence
-- runMaybeM = \case
--   (Just mia) -> Just <$> mia
--   Nothing -> pure Nothing
-- runMaybeM :: Monad m => Maybe (m a) -> m (Maybe a)

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


fieldValueM :: forall m a . (Monad m, Show a, FieldGetterM m a) =>
  NioFieldError ->
  NioValidateField a ->
  String ->
  FormInput ->
  m (Either FieldEr a)
fieldValueM b' validate key input = do
  let val'' = case filter ((== key) . fst) input of
        (v':[]) -> pure $ snd v'
        _ -> Nothing
  val <- runMaybeM (getFieldM <$> val'')
  case (validate (val ) key) of
    Nothing -> case val of
      Just (Right x) -> pure $ Right x
      Just (Left e) -> pure $ Left (key, NioFieldErrorV e)
      Nothing -> pure $ Left $ (key, b')
    Just (s, e) -> pure $ Left (s,e)
