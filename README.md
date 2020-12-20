# NiobiumCoconut

This is a library I started ages as a side project of sorts. It basically started after running into some frustration with the digestive-functors library.

It's quite a flexible form validation library (at least to anything I've ever used in the past). You can validate practically anything, and even use monadic contexts (like IO) for validation.

Hope to write a tutorial soon :).

There is set of tests written - that is probably the best place to start if you'd like to investigate this.

## Limitations

Hmmm I suppose it only supports key-value based input so basically what you get with data from web forms.

## Examples

### Projects that use this library:

- https://github.com/chrissound/BerylliumBlueberry

### Example code

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
