{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MyNioFieldError where

--import           Data.Text (Text)
--import NioFormTypes
--import Control.Monad.Identity
import NioFormTypes
import Data.Maybe
import Data.Either


data MyNioFieldError =
    MyNioFieldErrorNotPresent
  | MyNioIncorrectValue String String
  | MyNioFailedValidation String
  | MyNioFieldInternalFailure
  | MyNioFieldInternalFailureInfo String
  | MyNioFieldErrorMultiple ([MyNioFieldError])  deriving (Show, Eq)

instance Semigroup MyNioFieldError where
  (<>) a b = MyNioFieldErrorMultiple [a,b]

instance Monoid MyNioFieldError where
  mconcat (a:[])= a
  mconcat v = MyNioFieldErrorMultiple v
  mempty = MyNioFieldInternalFailure 

humanFriendMyNioFieldErrrorMsg :: MyNioFieldError -> String
humanFriendMyNioFieldErrrorMsg MyNioFieldErrorNotPresent = "This value was not submitted. This may be an internal error."
humanFriendMyNioFieldErrrorMsg (MyNioIncorrectValue _ v) = "Failed to decode: (" ++ v ++ ")."
humanFriendMyNioFieldErrrorMsg (MyNioFailedValidation v) = "Failed a validation rule: (" ++ v ++ ")."
humanFriendMyNioFieldErrrorMsg MyNioFieldInternalFailure = "Internal error when decoding this value."
humanFriendMyNioFieldErrrorMsg (MyNioFieldInternalFailureInfo _) = "Internal error when decoding this value."
humanFriendMyNioFieldErrrorMsg (MyNioFieldErrorMultiple v) = "Multiple errors: " ++ mconcat (fmap humanFriendMyNioFieldErrrorMsg v)

--instance FieldGetterErrorMsg Identity MyNioFieldError where
  --renderErrors _ = pure undefined
  --
  --
--instance [># OVERLAPPING #<] (Monad m, FieldGetter m a MyNioFieldError String) => FieldGetter m [a] MyNioFieldError String where
  ------getField k i = pure $ Just $ sequence . (fmap f) $ valuesForKey  where
  --getField k i = do
    --let zz = fzz valuesForKey
    --fmap catMaybes (sequence zz) >>= \case
      --[] -> pure Nothing
      --vv -> pure $ Just $ mylefts vv
      --where
    --valuesForKey :: FormInput
    --valuesForKey = filter ((k ==) . fst) i
    --fzz :: FormInput -> [m (Maybe (Either (String, [FieldEr MyNioFieldError]) a))]
    --fzz x = (\xx -> getField k [xx]) <$> x

getFieldList :: forall m a e. (Monad m, FieldGetter m a e String) => String -> FormInput -> m (Maybe (Either (String, [FieldEr e]) [a]))
getFieldList k i = do
    fmap catMaybes (sequence $ fzz valuesForKey) >>= \case
      [] -> pure $ Just $ Right []
      vv -> pure $ Just $ mylefts vv
      where
    valuesForKey :: FormInput
    valuesForKey = filter ((k ==) . fst) i
    fzz :: FormInput -> [m (Maybe (Either (String, [FieldEr e]) a))]
    fzz x = (\xx -> getField k [xx]) <$> x

mylefts :: Monoid a => [Either a b] ->  Either a [b]
mylefts v = case lefts v of
              [] -> Right $ rights v
              e -> Left $ mconcat $ e

--getFieldIdentityListString :: (Show a1, Show a2, Applicative f, Read a1, Eq a2) => a2
                                    --  -> [(a2, String)]
                                    --  -> f (Maybe (Either (a2, [(a2, MyNioFieldError)]) [a1]))
--getFieldIdentityListString k i = pure $ Just $ sequence . (fmap f) $ snd <$> filter ((k ==) . fst) (i) where
     --f x = case readMaybe x of
       --Just x' -> Right $ x'
       --Nothing -> Left (k, [(k, MyNioIncorrectValue (show k) x)])
