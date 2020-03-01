{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NioFormM where

import Types

class (Monad m) => FieldGetterM m a where
  getFieldM :: String -> m a

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
  case (validate (val :: Maybe a) key) of
    Nothing -> case val of
      Just x -> pure $ Right x
      Nothing -> pure $ Left $ (key, b')
    Just (s, e) -> pure $ Left (s,e)
