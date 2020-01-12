# Signaling adversity



### Maybe a, Either a b

```haskell
data Maybe a = Nothing | Just a

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
if even n then Just (n+2) else Nothing
```



```haskell
data Either a b = Left a | Right b
```

