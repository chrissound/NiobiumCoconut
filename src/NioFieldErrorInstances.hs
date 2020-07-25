{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS -Wno-orphans #-}
module NioFieldErrorInstances where

import NioFormTypes
import Control.Applicative

instance Alternative (Either NioFieldError) where
    (<|>) a b = case (a, b) of
                  (Right x, Right _) -> Right x
                  (_, Right y) -> Right y
                  (Right x, _) -> Right x
                  (_, _) -> empty
    empty = Left $ NioFieldErrorV "alternativ empty"

