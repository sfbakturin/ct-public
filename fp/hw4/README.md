# Домашнее задание №4

**Note**. You may notice that these tasks mention some data types from [homework 3](../hw3/). They are also defined in the template for this homework, namely in `HW4.Types` module, so you don't need to implement it there. However, if you think that some other parts of [hw3](../hw3/) may be useful in this homework (e.g. you want to use `mapExcept` to implement `mapExceptState`), feel free to define these functions here as well.

## Задание 1

1. Create a module named `HW4.T1` and define the following data type in it:

    ```haskell
    data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }
    ```

    This type is a combination of `Except` and `State`, allowing a stateful computation to abort with an error.

2. Implement the following functions:

    ```haskell
    mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
    wrapExceptState :: a -> ExceptState e s a
    joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
    modifyExceptState :: (s -> s) -> ExceptState e s ()
    throwExceptState :: e -> ExceptState e s a
    ```

    Using those functions, define `Functor`, `Applicative`, and `Monad` instances.

3. Using `do`-notation for `ExceptState` and combinators we defined for it (`pure`, `modifyExceptState`, `throwExceptState`), define the evaluation function:

    ```haskell
    data EvaluationError = DivideByZero
    eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
    ```

    It works just as `eval` from the [previous homework](../hw3/) but aborts the computation if division by zero occurs:

      * ```haskell
        runES (eval (2 + 3 * 5 - 7)) []
          ≡
        Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5])
        ```

      * ```haskell
        runES (eval (1 / (10 - 5 * 2))) []
          ≡
        Error DivideByZero
        ```

## Задание 2

1. Create a module named `HW4.T2` and define the following data type in it:

    ```haskell
    data ParseError = ErrorAtPos Natural

    newtype Parser a = P (ExceptState ParseError (Natural, String) a)
      deriving newtype (Functor, Applicative, Monad)
    ```

2. Implement the following function:

    ```haskell
    runP :: Parser a -> String -> Except ParseError a
    ```

3. Let us define a parser that consumes a single character:

    ```haskell
    pChar :: Parser Char
    pChar = P $ ES $ \(pos, s) ->
      case s of
        []     -> Error (ErrorAtPos pos)
        (c:cs) -> Success (c :# (pos + 1, cs))
    ```

    Study this definition:
      * What happens when the string is empty?
      * How does the parser state change when a character is consumed?
    It's very helpful to understand this example before starting to implement more complex parsers.

4. Implement a parser that always fails:

    ```haskell
    parseError :: Parser a
    ```

    Define the following instance:

    ```haskell
    instance Alternative Parser where
      empty = ...
      (<|>) = ...

    instance MonadPlus Parser   -- No methods.
    ```

    So that `p <|> q` tries to parse the input string using `p`, but in case of failure tries `q`.

    Make sure that the laws hold:

    ```haskell
    empty <|> p  ≡  p
    p <|> empty  ≡  p
    ```

    **Note**. It's not a mistake. You actually don't need to implement any methods in `MonadPlus` instance for `Parser` since all of them have a default implementation.

5. Implement a parser that checks that there is no unconsumed input left (i.e. the string in the parser state is empty), and fails otherwise:

    ```haskell
    pEof :: Parser ()
    ```

6. Study the combinators provided by `Control.Applicative` and `Control.Monad`. The following are of particular interest:

    * `msum`
    * `mfilter`
    * `optional`
    * `many`
    * `some`
    * `void`

    We can use them to construct more interesting parsers. For instance, here is a parser that accepts only non-empty sequences of uppercase letters:

      ```haskell
      pAbbr :: Parser String
      pAbbr = do
        abbr <- some (mfilter Data.Char.isUpper pChar)
        pEof
        pure abbr
      ```

    It can be used as follows:

      ```haskell
      ghci> runP pAbbr "HTML"
      Success "HTML"

      ghci> runP pAbbr "JavaScript"
      Error (ErrorAtPos 1)
      ```

7. Using parser combinators, define the following function:

    ```haskell
    parseExpr :: String -> Except ParseError Expr
    ```

    It must handle floating-point literals of the form `4.09`, the operators `+`, `-`, `*`, `/` with the usual precedence (multiplication and division bind tighter than addition and subtraction), and parentheses.

    Example usage:

    * ```haskell
      ghci> parseExpr "3.14 + 1.618 * 2"
      Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))
      ```

    * ```haskell
      ghci> parseExpr "2 * (1 + 3)"
      Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0)))))
      ```

    * ```haskell
      ghci> parseExpr "24 + Hello"
      Error (ErrorAtPos 3)
      ```

    The implementation must not use the `Read` class, as it implements similar functionality (the exercise is to write your own parsers). At the same time, you are encouraged to use existing `Applicative` and `Monad` combinators, since they are not specific to parsing.
