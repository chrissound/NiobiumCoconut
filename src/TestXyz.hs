module TestXyz where

--import NioForm
-- import NioFormInstances
import           NioFormTypes
import           Data.Text
import           Data.Foldable

data MyNioFieldError =
    MyNioFieldErrorEmty
  | MyNioIncorrectValue Text
  | MyNioFieldInternalFailure deriving (Show, Eq)



-- myGetField :: (Show a, FieldGetter  a) =>
--      NioValidateField a
--   -> NioFormKey
--   -> FormInput
--   -> Either (FieldEr) a
-- myGetField = fieldValue

data TestForm = TestForm Text Text Bool Int [String] deriving (Read, Show, Eq)
data TestForm2 = TestForm2 Text Text deriving (Read, Show, Eq)

isPresent :: NioValidateField a
isPresent v = case v of
  Just x  -> Right x
  Nothing -> Left (NioFieldErrorV $ MyNioFieldErrorEmty)

isEq :: (a -> Bool) -> Text -> NioValidateField a
isEq f failMsg v = case v of
  Just (v') -> case f v' of
    True  -> Right v'
    False -> Left (NioFieldErrorV $ MyNioIncorrectValue failMsg)
  Nothing -> Left (NioFieldErrorV $ MyNioFieldErrorEmty)

-- allRules ::  [Maybe b -> a -> Maybe (a, NioFieldError)] -> Maybe b -> a -> Maybe (a, NioFieldError)
-- allRules r v k = asum $ fmap (\r' -> r' v k) r

allRules :: [NioValidateField c] -> NioValidateField c
allRules r v = asum $ fmap (\r' -> r' v) r
