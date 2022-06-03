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

module NioFormGeneralized where

import           NioFormTypes
import           NioFormTypesGeneralized
import           NioForm hiding (runInputForm, runInputForm')
import           Control.Monad.Identity

runInputForm
  :: (Functor c)
  => (NioFormG c e)
  -> (FormInput -> Either [FieldEr e] b)
  -> FormInput
  -> Either (NioFormG c e) b
runInputForm nf vvv formInput = runIdentity $ runInputForm' nf (pure . vvv) formInput

runInputForm'
  :: forall m a e c. (Functor c, Monad m)
  => (NioFormG c e)
  -> (FormInput -> m (Either [FieldEr e] a))
  -> FormInput
  -> m (Either (NioFormG c e) a)
runInputForm' nf vvv formInput = do
  x <- (vvv formInput :: m (Either [FieldEr e] a))
  case x of
    Right x' -> pure $ pure x'
    Left e -> pure $ Left $ NioFormG
      { fieldsG = fmap (hydrateValues formInput . hydrateErrors e) $ fieldsG nf }
