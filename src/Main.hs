{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Text (Text)
-- import Yesod.Form.Types
import Control.Monad
import Control.Applicative
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
  getField' _ = True

instance FieldGetter Text where
  getField' _ = ""

instance FieldGetter Int where
  getField' _ = 123

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

inputTest :: [String] -> Either Int TestForm
inputTest = (liftM4 TestForm)
  <$> (getField ((==) "test") testForm "Test")
  <*> (getField (const True) testForm "Test2")
  <*> (getField (const True) testForm "Test3")
  <*> (getField ((==) 5) testForm "Test4")

getField :: FieldGetter a => (a -> Bool) -> NioForm -> String -> [String] -> Either Int a
getField validation nf x y = do
  let nf' = head $ fields nf
  (\(NioFieldView _ _ _ _ _) -> do
     let v = (getField' "test")
     case validation v of
       True -> Right v
       False -> Right v
    )
    nf'

