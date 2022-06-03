{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TestXyz where

--import NioForm
-- import NioFormInstances
import           NioFormTypes
--import           NioFieldErrorInstances
import           Data.Text (Text)
import           Data.Foldable
import Control.Applicative
import MyNioFieldError
import Data.String.Conversions


--instance Alternative (Either (NioFieldError MyNioFieldError)) where
    --(<|>) a b = case (a, b) of
                  --(Right x, Right _) -> Right x
                  --(_, Right y) -> Right y
                  --(Right x, _) -> Right x
                  --(_, _) -> empty
    --empty = Left $ NioFieldErrorV $ MyNioFieldInternalFailure


-- myGetField :: (Show a, FieldGetter  a) =>
--      NioValidateField a
--   -> NioFormKey
--   -> FormInput
--   -> Either (FieldEr) a
-- myGetField = fieldValue

data TestForm = TestForm Text Text Bool Int [String] deriving (Read, Show, Eq)
data TestForm2 = TestForm2 Text Text deriving (Read, Show, Eq)

isPresent :: NioValidateField a MyNioFieldError
isPresent v = case v of
  Just x  -> Right x
  Nothing -> Left (MyNioFieldErrorNotPresent)

isEq :: (a -> Bool) -> Text -> NioValidateField a MyNioFieldError
isEq f failMsg v = case v of
  Just (v') -> case f v' of
    True  -> Right v'
    False -> Left (MyNioFailedValidation $ cs failMsg)
  Nothing -> Left (MyNioFieldErrorNotPresent)

-- allRules ::  [Maybe b -> a -> Maybe (a, NioFieldError)] -> Maybe b -> a -> Maybe (a, NioFieldError)
-- allRules r v k = asum $ fmap (\r' -> r' v k) r

allRules :: Alternative (Either e) => [NioValidateField c e] -> NioValidateField c e
allRules r v = asum $ fmap (\r' -> r' v) r
