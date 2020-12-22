module NioFormTypesGeneralized where

import NioFormTypes

data NioFormG a = NioFormG {
  fieldsG :: a NioFieldView
  }
