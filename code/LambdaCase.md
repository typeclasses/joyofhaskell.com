```haskell
lambdaize = fmap f
  where
    f 'l' = 'λ'
    f x   = x
```

```haskell
{-# LANGUAGE LambdaCase #-}
lambdaize = fmap (\case 'l' -> 'λ'
                        x   -> x)
```

```haskell
λ> putStrLn (lambdaize "Delightful!")
-- Deλightfuλ!
```
