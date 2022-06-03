{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Options -Wno-unused-matches #-}

module NioForm where

import           Data.Text                      ( Text )
import           Data.Maybe                     ( catMaybes )
import           Data.String.Conversions
import           NioFormTypes
import           Control.Monad.Identity
import           Data.Proxy

runInputForm
  -- :: forall b a s . (FieldGetter Identity a s)
  ::
     NioForm e
  -> (FormInput -> Either [FieldEr e] b)
  -> FormInput
  -> Either (NioForm e) b
runInputForm nf vvv formInput = runIdentity $ runInputForm' nf (pure . vvv) formInput

-- myRunFormAppActionM 
--   :: forall m m2 a s . (FieldGetter m a s, Monad m2, m ~ m2)
--   => NioForm
--   -> (FormInput -> m (Either [FieldEr] a))
--   -> FormInput
--   -> m2 (Either NioForm a)
-- myRunFormAppActionM = runInputForm'

runInputForm'
  -- :: forall m a s . (FieldGetter m a s)
  :: forall m a e . (Monad m) => (NioForm e)
  -> (FormInput -> m (Either [FieldEr e] a))
  -> FormInput
  -> m (Either (NioForm e) a)
runInputForm' nf vvv formInput = do
  x <- (vvv formInput :: m (Either [FieldEr e] a))
  case x of
    Right x' -> pure $ pure x'
    Left e -> pure $ Left $ NioForm
       { fields = fmap (hydrateValues formInput . hydrateErrors e) $ fields nf }


hydrateValues :: FormInput -> NioFieldView e -> NioFieldView e
hydrateValues fi nf = nf
  { fvValue = do
                case filter ((== (fvId nf)) . cs . fst) fi of
                  []       -> fvValue nf -- original value
                  (v : []) -> NioFieldValS $ snd v
                  (v     ) -> NioFieldValM $ (fmap snd v)
  }

hydrateErrors
  :: forall a e
   . (Eq a, ConvertibleStrings Text a, Show a)
  => [(a, e)]
  -> NioFieldView e
  -> NioFieldView e
hydrateErrors e nf =
  nf { fvErrors = snd <$> filter ((==) (cs (fvId nf)) . fst) e }

getFormErrors :: t -> [t -> Either a b] -> [a]
getFormErrors input = catMaybes . fmap
  (\x -> case x input of
    Right _ -> Nothing
    Left  e -> Just e
  )

getFormErrorsM :: Monad m => v -> [v -> m (Either a b)] -> m [a]
getFormErrorsM fv l = do
  vars <- sequence $ (\x -> x fv) <$> l
  pure
    .   catMaybes
    $   (\x -> case x of
          Right _ -> Nothing
          Left  e -> Just e
        )
    <$> vars

fieldValue'
  :: forall m a e s .
    ( FieldGetter m a e s
    , FieldGetterErrorKey m a s
    , FieldGetterErrorMsg m e)
  => NioValidateField a e
  -> s
  -> FormInput
  -> m (Either (FieldEr e) a)
fieldValue' validate s input = do
  getField s input >>= \case
    Just (Right x) -> case (validate (Just x)) of
                 Right x' -> pure $ Right x'
                 Left e -> do
                   k <- getFieldErrorKey s (Proxy :: Proxy a)
                   pure $ Left (k, e)
    Nothing -> case (validate (Nothing)) of
                 Right x' -> pure $ Right x'
                 Left e -> do
                   k <- getFieldErrorKey s (Proxy :: Proxy a)
                   pure $ Left (k, e)
    Just (Left (k,e')) -> do
      x <- renderErrors e'
      pure $ Left (k, x)

fieldValue
  :: ( FieldGetter Identity a e s
     , FieldGetterErrorKey Identity a s
     , FieldGetterErrorMsg Identity e)
  => NioValidateField a e
  -> s
  -> FormInput
  -> Either (FieldEr e) a
fieldValue validate s input = runIdentity (fieldValue' validate s input)


mydbg'' :: Show a => String -> a -> a
-- mydbg'' s = traceShow
--   <$> ((++) s . show)
--   <*> id
mydbg'' _ = id
