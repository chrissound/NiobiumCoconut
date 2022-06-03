module NioFormTypesGeneralized where

import NioFormTypes

data NioFormG a e = NioFormG {
  fieldsG :: a (NioFieldView e)
  }
