{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Text (Text)
-- import Yesod.Form.Types
import Data.Bool 
import Control.Monad
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.String.Conversions
--import Data.Proxy

main :: IO ()
main = renderNioForm testForm


-- data FormInput = FormInput [(Text, Text)]

-- data Form = Form
-- data FormInputField = FormInputBool Bool | FormInputText | FormInputMultiple
-- data FormInputLable = FormInputLable Text

data NioForm = NioForm {
  fields :: [NioFieldView]
  }

data NioFieldError = NioFieldErrorEmty Text

data NioFieldInput =
    NioFieldInputHidden
  | NioFieldInputText
  | NioFieldInputMultiple [(Text,Text)]
  | NioFieldInputDigit
  deriving Show

data NioFieldView = forall a. Show a => NioFieldView
    { fvLabel :: Text
    , fvId :: Text
    , fvErrors :: [NioFieldError]
    , fvType :: NioFieldInput
    , fvValue :: a
    }

instance Show NioFieldView where
  show NioFieldView { fvValue = x, fvType = y} = do
    mconcat [show x, " ", show y]

class FieldGetter a where
  getField' :: String ->  a

instance FieldGetter Bool where
  getField' "true" = True
  getField' _ = False

instance FieldGetter Text where
  getField' = cs

instance FieldGetter Int where
  getField' = read

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) $ \x -> do
  print x

testForm :: NioForm
testForm = NioForm [
    NioFieldView "Test" "test" []   NioFieldInputText "test"
  , NioFieldView "Test2" "test2" [] NioFieldInputText "test"
  , NioFieldView "Test3" "test3" []
    (NioFieldInputMultiple [("a","a"), ("b","b")])
    "test"
  , NioFieldView "Test4" "test4" [] NioFieldInputDigit 5
  ]

data TestForm = TestForm Text Text Bool Int
data TestForm2 = TestForm2 Text
data TestFormFields =
    TestFormField1
  | TestFormField2
  | TestFormFieldBool3
  | TestFormFieldInt4

data MyEither a b = MyLeft a | MyRight b
type FieldEr = (String, NioFieldError)
type FormInput = [(String, String)]

runInputForm :: NioForm -> FormInput -> Either NioForm TestForm
runInputForm nf = (\case
  Right x -> Right x
  Left e -> Left $ NioForm { fields = fmap (f e) (fields nf)}
  ) . inputTest
  where
    f e nfv = nfv {
      fvErrors = (snd) <$> (filter (\(s,_) -> s == cs (fvId nfv)) e)
      }

inputTest :: FormInput -> Either ([FieldEr]) TestForm
inputTest = do
  ((liftM4 TestForm) <$> a <*> b <*> c <*> d) >>= \case
    Right x' -> pure $ pure (x')
    Left _ -> (\z -> do
                   Left $ mconcat [
                       getFormErrors z [a]
                     , getFormErrors z [b]
                     , getFormErrors z [c]
                     , getFormErrors z [d]
                     ]
      )
  where
      a = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) "test")) ""
      b = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) "test")) ""
      c = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) True)) ""
      d = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) 5)) ""
      errr x e = (bool (Nothing) (Just (x, e)))

getFormErrors :: FormInput -> [FormInput -> Either (FieldEr) a] -> [FieldEr]
getFormErrors input functions = catMaybes $ (
  \x -> case x input of
    Right _ -> Nothing
    Left e -> Just e
  ) <$> functions

getField :: FieldGetter a => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
getField validate x y = do
  let val = Just (getField' x)
  case validate (val) (x) of
    Nothing -> Right (getField' x)
    Just (s, e) -> Left (s,e)

