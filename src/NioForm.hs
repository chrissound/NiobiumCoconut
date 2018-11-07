{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module NioForm where

import Data.Text (Text)
import Data.Bool 
import Control.Monad
import Data.Maybe (catMaybes)
import Data.String.Conversions

data NioForm = NioForm {
  fields :: [NioFieldView]
  } deriving Show

data NioFieldError = NioFieldErrorEmty Text | NioFieldInternalFailure

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
  getField' _ = 123

data MyEither a b = MyLeft a | MyRight b
type FieldEr = (String, NioFieldError)
type FormInput = [(String, String)]

data TestForm = TestForm Text Text Bool Int deriving Show
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
      a = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) "test")) "f1"
      b = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) "test")) "f2"
      c = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) True)) "f3"
      d = getField (\x e -> x >>= (errr e (NioFieldErrorEmty "not test") ) . ((==) 5)) "f4"
      errr x e = bool (Just (x, e)) Nothing

getFormErrors :: FormInput -> [FormInput -> Either (FieldEr) a] -> [FieldEr]
getFormErrors input functions = catMaybes $ (
  \x -> case x input of
    Right _ -> Nothing
    Left e -> Just e
  ) <$> functions

getField :: FieldGetter a => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
getField validate key y = do
  let val = case filter ((== key) . fst) y of
        (v':[]) -> pure $ getField' $ fst v'
        _ -> Nothing
  case validate (val) (key) of
    Nothing -> case val of
      Just x -> Right x
      Nothing -> Left $ (key, NioFieldInternalFailure)
    Just (s, e) -> Left (s,e)
