# Первое домашнее задание

## Задание 1

1. Create a module named `HW1.T1` and define the following data type in it:

    ```haskell
    data Day = Monday | Tuesday | ... | Sunday
    ```

    (Obviously, fill in the `...` with the rest of the week days).

    Do not derive `Enum` for `Day`, as the derived `toEnum` is partial:

    ```haskell
    ghci> toEnum 42 :: Day
    *** Exception: toEnum{Day}: tag (42) is outside of enumeration's range (0,6)
    ```

2. Implement the following functions:

    ```haskell
    -- | Returns the day that follows the day of the week given as input.
    nextDay :: Day -> Day

    -- | Returns the day of the week after a given number of days has passed.
    afterDays :: Natural -> Day -> Day

    -- | Checks if the day is on the weekend.
    isWeekend :: Day -> Bool

    -- | Computes the number of days until Friday.
    daysToParty :: Day -> Natural
    ```

    In `daysToParty`, if it is already `Friday`, the party can start immediately, we don't have to wait for the next week (i.e. return `0` rather than `7`).

    Good job if you spotted that this task is a perfect fit for modular arithmetic, but that is not the point of the exercise. The functions must be implemented by operating on `Day` values directly, without conversion to a numeric representation.

## Задание 2

1. Create a module named `HW1.T2` and define the following data type in it:

    ```haskell
    data N = Z | S N
    ```

2. Implement the following functions:

    ```haskell
    nplus :: N -> N -> N        -- addition
    nmult :: N -> N -> N        -- multiplication
    nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
    ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
    ```

    The operations must be implemented without using built-in numbers (`Int`, `Integer`, `Natural`, and such).

3. Implement the following functions:

    ```haskell
    nFromNatural :: Natural -> N
    nToNum :: Num a => N -> a
    ```

4. (Advanced) Implement the following functions:

    ```haskell
    nEven, nOdd :: N -> Bool    -- parity checking
    ndiv :: N -> N -> N         -- integer division
    nmod :: N -> N -> N         -- modulo operation
    ```

    The operations must be implemented without using built-in numbers.

    In `ndiv` and `nmod`, the behavior in case of division by zero is not specified (you can throw an exception, go into an infinite loop, or delete all files on the computer).

### Задание 3

1. Create a module named `HW1.T3` and define the following data type in it:

    ```haskell
    data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
    ```

    The `Meta` field must store additional information about the subtree that can be accessed in constant time, for example its size. You can use `Int`, `(Int, Int)`, or a custom data structure:

    ```haskell
    type Meta = Int         -- OK
    data Meta = M Int Int   -- OK
    ```

    Functions operating on this tree must maintain the following invariants:

    1. **Sorted**: The elements in the left subtree are less than the head element of a branch, and the elements in the right subtree are greater.
    2. **Unique**: There are no duplicate elements in the tree (follows from **Sorted**).
    3. **CachedSize**: The size of the tree is cached in the `Meta` field for constant-time access.
    4. (Advanced) **Balanced**: The tree is balanced according to one of the following strategies:
        * **SizeBalanced**: For any given `Branch _ l _ r`, the ratio between the size of `l` and the size of `r` never exceeds `3`.
        * **HeightBalanced**: For any given `Branch _ l _ r`, the difference between the height of `l` and the height of `r` never exceeds `1`.

    These invariants enable efficient processing of the tree.

2. Implement the following functions:

    ```haskell
    -- | Size of the tree, O(1).
    tsize :: Tree a -> Int

    -- | Depth of the tree.
    tdepth :: Tree a -> Int

    -- | Check if the element is in the tree, O(log n)
    tmember :: Ord a => a -> Tree a -> Bool

    -- | Insert an element into the tree, O(log n)
    tinsert :: Ord a => a -> Tree a -> Tree a

    -- | Build a tree from a list, O(n log n)
    tFromList :: Ord a => [a] -> Tree a
    ```

    Tip 1: in order to maintain the **CachedSize** invariant, define a helper function:

    ```haskell
    mkBranch :: Tree a -> a -> Tree a -> Tree a
    ```

    Tip 2: the **Balanced** invariant is the hardest to maintain, so implement it last. Search for "tree rotation".
