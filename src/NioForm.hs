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
  :: forall a s . FieldGetter Identity a s
  => NioForm
  -> (FormInput -> Either [FieldEr] a)
  -> FormInput
  -> Either NioForm a
runInputForm nf vvv formInput = runIdentity $ runInputForm' nf (pure . vvv) formInput

runInputForm'
  :: forall m a s . FieldGetter m a s
  => NioForm
  -> (FormInput -> m (Either [FieldEr] a))
  -> FormInput
  -> m (Either NioForm a)
runInputForm' nf vvv formInput = do
  x <- (vvv formInput :: m (Either [FieldEr] a))
  case x of
    Right x' -> pure $ pure x'
    Left e -> pure $ Left $ NioForm
      { fields = fmap (hydrateValues formInput . hydrateErrors e) $ fields nf }


hydrateValues :: FormInput -> NioFieldView -> NioFieldView
hydrateValues fi nf = nf
  { fvValue = do
                case filter ((== (fvId nf)) . cs . fst) fi of
                  []       -> fvValue nf -- original value
                  (v : []) -> NioFieldValS $ snd v
                  (v     ) -> NioFieldValM $ (fmap snd v)
  }

hydrateErrors
  :: forall a
   . (Eq a, ConvertibleStrings Text a, Show a)
  => [(a, NioFieldError)]
  -> NioFieldView
  -> NioFieldView
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
  :: forall m a s . (Show a, FieldGetter m a s)
  => NioValidateField a
  -> s
  -> FormInput
  -> m (Either (FieldEr) a)
fieldValue' validate s input = do -- validate key input
  --let val'' = case filter ((== key) . fst) input of
        --(v:[]) -> pure $ NioFieldValS $  snd v
        --[] -> Nothing
        --(v) -> pure $ NioFieldValM (fmap snd v)
  --let val = (\val'' -> case val'' of
                         --NioFieldValS yay -> getField yay
                         --NioFieldValM yay -> _getFieldMany <$> yay
            --) <$> val''
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
    Just (Left (e,e')) -> pure $ Left (e, NioFieldErrorV e')
  --case val of
    --Just (Right x) -> Right x
    --Just (Left e) -> Left $ (key, NioFieldErrorV e)
    --Nothing -> Left $ (key, nioerrorFailRetriveOrError)
  --case validate (mydbg'' "fieldValue val" val) key of
    --Nothing -> case val of
      --Just (Right x) -> Right x
      --Just (Left e) -> Left $ (key, NioFieldErrorV e)
      --Nothing -> Left $ (key, nioerrorFailRetriveOrError)
    --Just (s, e) -> Left (s,e)

fieldValue
  :: (Show a, FieldGetter Identity a s)
  => NioValidateField a
  -> s
  -> FormInput
  -> Either (FieldEr) a
--fieldValue validate key input = undefined
fieldValue validate s input = runIdentity (fieldValue' validate s input)
-- fieldValue
--   :: (Show a, FieldGetter a)
--   => NioValidateField a
--   -> NioFormKey
--   -> FormInput
--   -> Either (FieldEr) a
-- fieldValue validate key input = undefined
  --let val'' = case filter ((== key) . fst) input of
        --(v:[]) -> pure $ NioFieldValS $  snd v
        --[] -> Nothing
        --(v) -> pure $ NioFieldValM (fmap snd v)
  --let val = (\val'' -> case val'' of
                         --NioFieldValS yay -> getField yay
                         --NioFieldValM yay -> _getFieldMany <$> yay
            --) <$> val''
  --case val of
    --Just (Right x) -> Right x
    --Just (Left e) -> Left $ (key, NioFieldErrorV e)
    --Nothing -> Left $ (key, nioerrorFailRetriveOrError)
  --case validate (mydbg'' "fieldValue val" val) key of
    --Nothing -> case val of
      --Just (Right x) -> Right x
      --Just (Left e) -> Left $ (key, NioFieldErrorV e)
      --Nothing -> Left $ (key, nioerrorFailRetriveOrError)
    --Just (s, e) -> Left (s,e)



mydbg'' :: Show a => String -> a -> a
-- mydbg'' s = traceShow
--   <$> ((++) s . show)
--   <*> id
mydbg'' _ = id

--class Monad m => FieldGetter' m a where
  --data FieldGetterSelector m a
  --getField' :: FieldGetterSelector m a -> FormInput -> m (Either [FieldEr] a)
  --getErrors' :: FieldGetterSelector m a -> FormInput -> m (Maybe [FieldEr])

--instance FieldGetter' Identity String where
  --data FieldGetterSelector Identity String = FieldGetterSelectorString String
  --getField' (FieldGetterSelectorString x) fi = pure $ Right x
  --getErrors' (FieldGetterSelectorString x) fi = pure $ Nothing

--instance FieldGetter' Identity (String, String) where
  --data FieldGetterSelector Identity (String, String) = FieldGetterSelectorStrings (String,String)
  --getField' (FieldGetterSelectorStrings x) fi = pure $ Right x
  --getErrors' (FieldGetterSelectorStrings x) fi = pure $ Nothing



--instance FieldGetter Identity String (String, String) where
  --getField (a,b) _ = pure $ Right a
  --getErrors'' (a,b) _ x = pure Nothing

-- runInputForm'
--   :: forall m a . FieldGetter' m a
--   => NioForm
--   -> (FormInput -> m (Either [FieldEr] a))
--   -> FormInput
--   -> m (Either NioForm a)
-- runInputForm' nf vvv formInput = do
--   x <- (vvv formInput :: m (Either [FieldEr] a))
--   case x of
--     Right x' -> pure $ pure x'
--     Left _ -> pure $ Left $ NioForm undefined


