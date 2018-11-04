{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
-- import Yesod.Form.Types
import Control.Monad

main :: IO ()
main = renderNioForm testForm


-- data FormInput = FormInput [(Text, Text)]

-- data Form = Form
-- data FormInputField = FormInputBool Bool | FormInputText | FormInputMultiple
-- data FormInputLable = FormInputLable Text

data NioForm = NioForm {
  fields :: [NioFieldView]
  }

data NioFieldError = NioFieldError Text
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

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) $ \x -> do
  print x

testForm :: NioForm
testForm = NioForm [
    NioFieldView "Test" "test" [] NioFieldInputText "test"
  , NioFieldView "Test2" "test2" [] NioFieldInputText "test"
  , NioFieldView "Test3" "test3" []
    (NioFieldInputMultiple [("a","a"), ("b","b")])
    "test"
  , NioFieldView "Test4" "test4" [] NioFieldInputDigit "0"
  ]

data TestForm = TestForm Text Text Bool

-- inputTest = TestForm
--   <$> getField "Test"
--   <*> getField "Test2"
--   <*> getField "Test3"
