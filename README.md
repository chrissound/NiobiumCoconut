# NiobiumCoconut

## Limitations

This is a library I started ages as a side project of sorts. I'm not actually sure how usable it is and whether it has bugs / limitations. I just decided to share it. It basically started after running into some frustration with the digestive-functors library.

It's quite a flexible form validation library. You can validate practically anything, and even use IO etc for validation, a field value can be validated by looking at all values of the input. It has someeee type cleverness using existential types.

I think it's pretty much complete. There is one niggly bit where I realized I should return a `Either e a` instead of `Maybe a` - or maybe it was `error` instead of `Maybe a`? Can't remember.

There is set of tests written - that is probably the best place to start if you'd like to investigate this.

Some example snippets, you basically need 3 things:
- The value itself `TestForm` in our case
- The `NioForm` that specifies type of input
- The 'run' function which takes some input, the `NioForm` thingy and gives you your result!

```

data TestForm = TestForm Text Text Bool Int deriving (Show, Eq)

data MyNioFieldError =
    MyNioFieldErrorEmty
  | MyNioIncorrectValue Text
  | MyNioFieldInternalFailure deriving (Show, Eq)

testForm :: NioForm
testForm = NioForm [
       NioFieldView "Test" "f1" emptyError   NioFieldInputText ""
     , NioFieldView "Test 2" "f2" emptyError NioFieldInputText ""
     , NioFieldView "Test 3" "f3" emptyError
        (NioFieldInputMultiple [("a","a"), ("b","b")])
        (show True)
     , NioFieldView "Test4" "f4" emptyError NioFieldInputDigit (show 0)
  ]

inputTestP' :: FormInput -> Either ([FieldEr]) TestForm
inputTestP' fi =
  (first $ const $ collect fi)
    (((liftM4 TestForm) <$> a <*> b <*> c <*> d) fi)
  where
      collect z = mconcat [
                          getFormErrors z [a]
                        , getFormErrors z [b]
                        , getFormErrors z [c]
                        , getFormErrors z [d]
                        ]

      a = myGetField (isPresent) "f1"
      b = myGetField (isPresent) "f2"
      c = myGetField
          (allRules[isPresent, isEq (== True) "Not true"]) "f3"
      d = myGetField
          (allRules[isPresent, isEq (== 4) "Not 4"]) "f4"
```
