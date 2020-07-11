module TestXyz where

import NioForm
-- import NioFormInstances
import NioFormTypes
import Data.Text
import Data.String.Conversions
import Data.Foldable

data MyNioFieldError =
    MyNioFieldErrorEmty
  | MyNioIncorrectValue Text
  | MyNioFieldInternalFailure deriving (Show, Eq)
 
myGetField :: (Show a, FieldGetter a) =>
     NioValidateField a
  -> NioFormKey
  -> FormInput
  -> Either (FieldEr) a
myGetField = fieldValue

data TestForm = TestForm Text Text Bool Int deriving (Show, Eq)
data TestForm2 = TestForm2 Text Text deriving (Show, Eq)

isPresent :: NioValidateField a
isPresent v k = case v of
  Just _ -> Nothing
  Nothing -> Just (k, NioFieldErrorV $ MyNioFieldErrorEmty)

isEq :: (a -> Bool)
  -> Text
  -> NioValidateField a
isEq rule failMsg v k = case v of
  Just (Right v') -> case rule v' of
    True ->             Nothing
    False ->            Just (k, NioFieldErrorV $ MyNioIncorrectValue failMsg)
  Just (Left x') ->     Just (k, NioFieldErrorV $ MyNioIncorrectValue $ cs x')
  Nothing ->            Just (k, NioFieldErrorV $ MyNioFieldErrorEmty )

-- allRules ::  [Maybe b -> a -> Maybe (a, NioFieldError)] -> Maybe b -> a -> Maybe (a, NioFieldError)
-- allRules r v k = asum $ fmap (\r' -> r' v k) r

allRules ::  [NioValidateField c] -> NioValidateField c
allRules r v k = asum $ fmap (\r' -> r' v k) r
