{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-unused-imports #-}
module Main where

import Data.Text (Text)
import Control.Monad
import Text.Pretty.Simple (pPrint)
import Debug.Trace
import Data.Foldable
import NioFormInstances

import NioForm

--data TestForm = TestForm Text Text Bool Int deriving Show
data TestForm = TestForm Text Text Text Text deriving Show
data TestForm2 = TestForm2 Text Text deriving Show

data MyNioFieldError =
    MyNioFieldErrorEmty 
  | MyNioIncorrectValue Text
  | MyNioFieldInternalFailure deriving Show


main :: IO ()
main = do
  pPrint $
    --runInputForm testForm inputTest [("f1", "test"), ("f3","true"), ("f4", "4")]
    -- runInputForm testForm inputTest [
    --     ("f1", "test")
    --   , ("f2", "wohoo")
    --   , ("f3","true")
    --   , ("f4", "4")
    -- ]
    ""

renderNioForm :: NioForm -> IO ()
renderNioForm nf = forM_ (fields nf) $ \x -> do
  print x

emptyError :: [NioFieldError]
emptyError = []

testForm :: NioForm
testForm = NioForm [
       NioFieldView "Test" "f1" emptyError   NioFieldInputText ""
     , NioFieldView "Test 2" "f2" emptyError NioFieldInputText ""
     , NioFieldView "Test 3" "f3" emptyError
        (NioFieldInputMultiple [("a","a"), ("b","b")])
        (show True)
     , NioFieldView "Test4" "f4" emptyError NioFieldInputDigit (show 0)
  ]

      -- <$> (fmap (pure :: Applicative f => a -> f a))
      -- <$> (fmap (pure :: Applicative f => a -> f a))
-- inputTest :: FormInput -> IO (Either ([FieldEr]) TestForm2)
-- inputTest = do
--           ((fmap pure) <$> (TestForm2))
--           <$> a <*> b
--       >>= \case
--         Right x -> pure . pure . pure (x)
--         Left _ -> undefined
--   where
--       a = const ("") -- :: FormInput -> IO (Either FieldEr a)
--       b = const ("") -- :: FormInput -> IO (Either FieldEr a)
-- grrrrrrrrrrrrrrrrrrrrrr
-- inputTest :: FormInput -> IO (Either ([FieldEr]) TestForm)
-- inputTest = do
--           ((fmap pure) <$> (TestForm))
--           <$> a <*> b <*> c <*> d
--inputTest :: FormInput -> IO (Either (String) (Int, Int, Int))

fuck :: a -> b -> Either String (a, b)
fuck = (fmap Right) <$> (,)

fuck2 :: a -> b ->  c -> Either String (a, b, c)
fuck2 x y z = (Right) $ (
    \a' b' c' -> (a', b', c')
  ) x y z

fuck3 :: a -> b -> Either String (a, b)
fuck3 = fmap Right <$> (,)

fuck4 :: Monad m => a -> b -> c -> m (a, b, c)
fuck4 = (fmap . fmap) pure <$> (,,)

fuck5 :: a -> b -> c -> d -> Either String (a, b, c, d)
fuck5 = (fmap . fmap . fmap) Right <$> (,,,)
--fuck5 = (fmap . fmap . fmap $ Right) <$> (\a' b' c' d'-> (a', b', c', d'))

inputTestW :: FormInput -> Either (String) (Int, Int, Int)
inputTestW = do
          (
              ( fuck4) 
              <$> (a :: FormInput -> Int)
              <*> (b:: FormInput -> Int)
              <*> (c:: FormInput -> Int)
            )
  where
      a = undefined -- (myGetFieldIO (isPresent) "f1")                        -- :: FormInput -> IO (Either FieldEr a)
      b = undefined -- pure . myGetField (isPresent) "f2"                    -- :: FormInput -> IO (Either FieldEr a)
      c = undefined -- pure . myGetField (isPresent) "f2"                    -- :: FormInput -> IO (Either FieldEr a)

-- wtf :: IO (Either () (String,String))
-- wtf = (liftM2) <$> a <*> b where
--   a = pure <$> getLine
--   b = pure <$> getLine

inputTest :: Monad m => FormInput -> m (Either ([FieldEr]) TestForm)
inputTest v = do
  (liftM4 (liftM4 TestForm) <$> a <*> b <*> c <*> d) v >>= \case
    Right x -> pure . pure $ x
    Left _ -> do
      e <- fmap mconcat $ sequence [
          getFormErrorsM v [a]
        , getFormErrorsM v [b]
        , getFormErrorsM v [c]
        , getFormErrorsM v [d]
        ]
      pure $ Left $ (e :: [FieldEr])
  where
    a = undefined .  Right  :: Monad m => FormInput -> m (Either FieldEr  a)
    b = undefined .  Right  :: Monad m => FormInput -> m (Either FieldEr  a)
    c = undefined .  Right  :: Monad m => FormInput -> m (Either FieldEr  a)
    d = undefined .  Right  :: Monad m => FormInput -> m (Either FieldEr  a)
  -- >>= \case
  --   Right z -> undefined
  --   Left z -> undefined
-- --inputTest :: FormInput -> IO (Either (String) (Int, Int, Int))
--       -- >>= \case
--       --   Right x' -> pure . pure . pure (x')
--       --   Left _ -> undefined
--       -- Left _ -> (\z -> do
--       --               Left $ traceShowId $ mconcat [
--       --                    getFormErrors z [a]
--       --                  , getFormErrors z [b]
--       --                  , getFormErrors z [c]
--       --                  , getFormErrors z [d]
--       --                  ]
--       --   )
--   where
--       a = undefined -- (myGetFieldIO (isPresent) "f1")                        -- :: FormInput -> IO (Either FieldEr a)
--       b = undefined -- pure . myGetField (isPresent) "f2"                    -- :: FormInput -> IO (Either FieldEr a)
--       c = undefined -- pure . myGetField
--           -- (allRules[isPresent, isEq (== True) "Not true"]) "f3" -- :: FormInput -> IO (Either FieldEr a)
--       d = undefined -- pure . myGetField
--           -- (allRules[isPresent, isEq (== 4) "Not 4"]) "f4"       -- :: FormInput -> IO (Either FieldEr a)

myGetField :: (Show a, FieldGetter a) => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> Either (FieldEr) a
myGetField = fieldValue (undefined)

myGetFieldIO :: (Show a, FieldGetter a) => (Maybe a -> String -> Maybe (FieldEr)) -> String -> FormInput -> IO (Either (FieldEr) a)
myGetFieldIO = fieldValueIO (undefined)

isPresent :: Maybe b -> a -> Maybe (a, NioFieldError)
isPresent x k = case x of
  Just _ -> Nothing
  Nothing -> Just (k, NioFieldErrorV $ MyNioFieldErrorEmty)

isEq :: (b -> Bool) -> Text -> Maybe b -> a -> Maybe (a, NioFieldError)
isEq f t x k = case x of
  Just x' -> if f x' then Nothing else Just (k, NioFieldErrorV $ MyNioIncorrectValue $  t)
  _ -> Just (k, NioFieldErrorV $ MyNioIncorrectValue "Not true")

allRules ::  [Maybe b -> a -> Maybe (a, NioFieldError)] -> Maybe b -> a -> Maybe (a, NioFieldError)
allRules r v k = asum $ fmap (\r' -> r' v k) r
