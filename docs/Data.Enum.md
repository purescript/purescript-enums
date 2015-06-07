## Module Data.Enum

#### `Cardinality`

``` purescript
newtype Cardinality a
  = Cardinality Int
```

#### `runCardinality`

``` purescript
runCardinality :: forall a. Cardinality a -> Int
```

#### `Enum`

``` purescript
class (Bounded a) <= Enum a where
  cardinality :: Cardinality a
  succ :: a -> Maybe a
  pred :: a -> Maybe a
  toEnum :: Int -> Maybe a
  fromEnum :: a -> Int
```

Type class for enumerations. This should not be considered a part of a
numeric hierarchy, ala Haskell. Rather, this is a type class for small,
ordered sum types with statically-determined cardinality and the ability
to easily compute successor and predecessor elements. e.g. `DayOfWeek`, etc.

Laws:

- ```succ bottom >>= succ >>= succ ... succ [cardinality - 1 times] == top```
- ```pred top    >>= pred >>= pred ... pred [cardinality - 1 times] == bottom```
- ```e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2```
- ```forall a > bottom: pred a >>= succ == Just a```
- ```forall a < top:  succ a >>= pred == Just a```
- ```pred >=> succ >=> pred = pred```
- ```succ >=> pred >=> succ = succ```
- ```toEnum (fromEnum a) = Just a```
- ```forall a > bottom: fromEnum <$> pred a = Just (fromEnum a - 1)```
- ```forall a < top:  fromEnum <$> succ a = Just (fromEnum a + 1)```

##### Instances
``` purescript
instance enumChar :: Enum Char
instance enumMaybe :: (Enum a) => Enum (Maybe a)
instance enumBoolean :: Enum Boolean
instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b)
instance enumEither :: (Enum a, Enum b) => Enum (Either a b)
```

#### `defaultSucc`

``` purescript
defaultSucc :: forall a. (Int -> Maybe a) -> (a -> Int) -> a -> Maybe a
```

```defaultSucc toEnum fromEnum = succ```

#### `defaultPred`

``` purescript
defaultPred :: forall a. (Int -> Maybe a) -> (a -> Int) -> a -> Maybe a
```

```defaultPred toEnum fromEnum = pred```

#### `defaultToEnum`

``` purescript
defaultToEnum :: forall a. (a -> Maybe a) -> a -> Int -> Maybe a
```

Runs in `O(n)` where `n` is `fromEnum a`

```defaultToEnum succ bottom = toEnum```

#### `defaultFromEnum`

``` purescript
defaultFromEnum :: forall a. (a -> Maybe a) -> a -> Int
```

Runs in `O(n)` where `n` is `fromEnum a`

```defaultFromEnum pred = fromEnum```

#### `enumFromTo`

``` purescript
enumFromTo :: forall a. (Enum a) => a -> a -> Array a
```

Property: ```fromEnum a = a', fromEnum b = b' => forall e', a' <= e' <= b': Exists e: toEnum e' = Just e```

Following from the propery of `intFromTo`, we are sure all elements in `intFromTo (fromEnum a) (fromEnum b)` are `Just`s.

#### `enumFromThenTo`

``` purescript
enumFromThenTo :: forall a. (Enum a) => a -> a -> a -> Array a
```

`[a,b..c]`

Correctness for using `fromJust` is the same as for `enumFromTo`.

#### `intFromTo`

``` purescript
intFromTo :: Int -> Int -> Array Int
```

Property: ```forall e in intFromTo a b: a <= e <= b```

#### `intStepFromTo`

``` purescript
intStepFromTo :: Int -> Int -> Int -> Array Int
```

Property: ```forall e in intStepFromTo step a b: a <= e <= b```


