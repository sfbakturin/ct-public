# Домашнее задание №6

## Задание №1: Многопоточная хэш-таблица

In this task, you need to implement a thread-safe separately chained hash table using STM.

The hash table is represented by `CHT` datatype:

```haskell
type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }
```

Where `chtBuckets` is an array of buckets having a length equal to the current capacity, and `chtSize` is the number of elements in the hash table.

Namely, you need to implement the following functions (Their types are simplified in this example. For the reference, see the homework template):

```haskell
newCHT  :: IO (ConcurrentHashTable k v)
getCHT  :: k -> ConcurrentHashTable k v -> IO (Maybe v)
putCHT  :: k -> v -> ConcurrentHashTable k v -> IO ()
sizeCHT :: ConcurrentHashTable k v -> IO Int
```

Your implementation should be:

* Must be resistant to asynchronous exceptions: asynchronous exceptions shouldn't make the hash table to be in inconsistent state.
* Have no deadlocks.
* Use `O(n)` space, where `n` stands for the number of elements.

All these properties can be achieved by correct usage of STM capabilities.

**Notes about implementation details** (you should follow them since they're checked in tests):

* To choose the bucket for key, use `hash(k) % capacity`
* You need to resize the table when `chtSize >= capacity * loadFactor`

**Note on STM**:

You may noticed that the type of `TVar` is a bit different from the one covered on lecture, and have additional `stm` type parameter. This is because this task uses STM primitives from the `concurrency` (not `stm`) library which are polymorphic by STM implementation.

These polymorphic primitives are useful for testing: testing library uses its own STM implementation to check the correctness of concurrent code.

These examples will help you to understand how equivalent regular and polymorphic type signatures look like:

`concurrency`:

```haskell
sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
```

```haskell
type BucketsArray stm k v = TArray stm Int (Bucket k v)
```

```haskell
foo :: MonadSTM stm => k -> BucketsArray stm k v -> stm Int
```

`stm`:

```haskell
sizeCHT :: STM (CHT k v) -> IO Int
```

```haskell
type BucketsArray k v = TArray Int (Bucket k v)
```

```haskell
foo :: BucketsArray k v -> STM Int
```

## Задание №2: Множество типов

In this task you need to implement the set of strings that stores its elements on the type-level.

```haskell
type TSet = [Symbol]
```

The `Symbol` type from `GHC.TypeLits` represents a type-level string. `TSet` is defined as a list of type-level strings.

Namely, you should implement the following operations (note that we use type families, since they operate with types, unlike the regular functions that operate with terms):

```haskell
type family Contains (name :: Symbol) (set :: TSet) :: Bool

type family Delete (name :: Symbol) (set :: TSet) :: TSet

type family Add (v :: Symbol) (set :: TSet) :: TSet
```

Ordering of elements of `TSet` is not required, as well as operations efficiency (i.e. they can have linear complexity). The only requirement is the uniqueness of the elements in `TSet`.

If these operations are implemented correctly, the following code should compile:

```haskell
data Proxy a = Proxy

type ExampleSet = '[ "a", "b" ] :: TSet

test1 :: Proxy 'True
test1 = Proxy @(Contains "a" ExampleSet)

test2 :: Proxy '[ "b" ]
test2 = Proxy @(Delete "a" ExSet)
```

## Задание №3: Комонада-19

In this task you need to implement the simulation of Covid-19 infection on a 2-dimensional grid. Namely, you need to implement the "Infection probability" model from [there](https://kapter.github.io/outbreak/) with the addition of the possibility of getting infected again.

Your implementation should have the following parameters:

* Infection probability (`0 < p < 1`)
* Duration of incubation period
* Illness duration
* Immunity duration (how long can a person not become infected again after recovery)

The general rule is that a person whose infection is in the incubation period or with active symptoms can transmit the infection to an uninfected neighbor with probability `p`.

To implement this model, you need to use `Grid` datatype and its comonadic properties:

```haskell
data ListZipper a = LZ [a] a [a]

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }
```

**Tip**: For random simulation, store the `StdGen` object in each cell and use it as a random generator every time you need to generate a random number in a cell. It's ok if identical `StdGen` objects are stored on the diagonals of the grid.

Since there are no automatic tests for this task, there are no strict requirements to the project structure. As a result, you need to implement the `simulate` function which takes the config and generates infinite list of grids.

Apart from that, you need to implement console application which uses the following command-line arguments and prints the simulation process (it should use `simulate` function) to terminal.

Command-line arguments:

```shell
--prob             # Infection probability
--incub            # Incubation period duration
--ill              # Illness duration
--immun            # Immunity duration
--grid-size        # Output grid size. Note that `Grid` type represents an infinite grid.
--iterations       # The number of simulation iterations
```

For different cell states use the following notation:

* `_` - healthy cell
* `i` - infected cell
* `#` - ill cell
* `@` - immune cell

**Tip:** Use `optparse-applicative` library for command-line arguments parsing.

Example of how the resulting console applicaiton may work:

```bash
stack exec -- comonad19 --prob 0.2 --incub 2 --ill 5 --immun 7 --grid-size 11 --iterations 10 
```

```repl
___________
___________
___________
___________
___________
_____i_____
___________
___________
___________
___________
___________



___________
___________
___________
___________
___________
_____i_____
____i______
___________
___________
___________
___________



___________
___________
___________
___________
___________
_____#i____
____ii_____
___________
___________
___________
___________



___________
___________
___________
___________
___________
____i#i____
____#ii____
___________
___________
___________
___________



___________
___________
___________
___________
____ii_i___
___ii##i___
___i##i____
___i_i_____
___________
___________
___________



___________
___________
___________
___________
____ii_ii__
___i###ii__
ii###i___
___i_ii____
___i_______
___________
___________



___________
___________
___________
____i_i_i
___i##_#ii_
i#####i
i####i___
__i#i#i____
__ii_______
___________
___________



___________
___________
____ii_i___
___iiiiiii_
__ii##_##ii
_ii##@###
######___
__i#i##____
_ii#ii_____
___________
___________



___________
___ii___i
iiiiiii_i
_iii#i#i#ii
iii###i###i
_i###@###i_
_i##@###i
_i#####i___
_i##iii____
i__i_____
___________



__i_ii___i_
_i_iii_ii
i_ii##i#iii
_ii#######i
ii####i####
i####@@##ii
_i##@@##iii
_i#####iii_
_#####iii__
__i_iiii___
_____ii____
```
