{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | The `ListT` type is like a list that lets you interleave effects between
--   each element of the list.
module List.Transformer
    (
      -- * Introduction
      -- $intro

      -- ** Example: stdin, stdout
      -- $standardStreams

      -- ** Core operations
      -- $core

      -- ** Monadic combination
      -- $monad

      -- ** Exercise: Interaction
      -- $interaction

      -- * ListT
      ListT(..)

      -- ** Consuming
      -- $pleaseStream
    , runListT
    , fold
    , foldM

      -- ** Constructing
      -- $constructing
    , select
    , unfold

      -- ** Removing elements
    , take
    , drop
    , dropWhile
    , takeWhile
      -- $filter

      -- ** Concatenation
      -- $concatenation

      -- ** Pairwise combination
      -- $pairwise
    , zip

      -- ** Repetition
      -- $repetition

      -- * Step
    , Step(..)

      -- * Alternative instances
    , ZipListT(..)

      -- * Re-exports
    , MonadTrans(..)
    , MonadIO(..)
    , Alternative(..)
    , MFunctor (..)
    ) where

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative(..), liftA2)
#else
import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Data.Foldable (Foldable)
import Data.Functor ((<$))
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable)
#endif
import Control.Monad (MonadPlus(..))
import Control.Monad.Error.Class (MonadError(..))
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Semigroup (Semigroup(..))
import Prelude hiding (drop, dropWhile, pred, take, takeWhile, zip)

import qualified Data.Foldable

-- $setup
-- >>> :set -XNoMonomorphismRestriction

{- $intro

The type's definition is very short:

@newtype 'ListT' m a = ListT { next :: m ('Step' m a) }@

    Every `ListT` begins with an outermost effect (the @\'m\'@, commonly 'IO'). The return value of that effect is either:

@data 'Step' m a = Cons a ('ListT' m a) | Nil@

    * Cons: a new list element followed by the rest of the list
    * Nil : an empty list

-}

{- $standardStreams

    You most commonly use the ListT when you wish to generate each element of
    the list using `IO`.  For example, you can read lines from standard input:

> import List.Transformer
>
> import qualified System.IO
>
> stdin :: ListT IO String
> stdin = ListT (do
>     eof <- System.IO.isEOF
>     if eof
>         then return Nil
>         else do
>             string <- getLine
>             return (Cons string stdin) )

    You can also loop over a `ListT` to consume elements one-at-a-time.  You
    \"pay as you go\" for effects, only running what you actually need:

> stdout :: ListT IO String -> IO ()
> stdout strings = do
>     s <- next strings
>     case s of
>         Nil                  -> return ()
>         Cons string strings' -> do
>             putStrLn string
>             stdout strings'

    Combining @stdin@ and @stdout@ forwards lines one-by-one from standard input
    to standard output:

> main :: IO ()
> main = stdout stdin

    These lines stream in constant space, never retaining more than one line in
    memory:

> $ runghc aboveExample.hs
> Test<Enter>
> Test
> 123<Enter>
> 123
> ABC<Enter>
> ABC
> <Ctrl-D>
> $

-}

{- $core

    The most important operations that you should familiarize yourself with are:

    * `empty`, which gives you an empty `ListT` with 0 elements

> empty :: ListT IO a

    * `pure` / `return`, which both convert a value into a one-element `ListT`

> pure, return :: a -> ListT IO a

    * `liftIO`, which converts an `IO` action into a one-element `ListT`

> liftIO :: IO a -> ListT IO a

    * (`<|>`), which concatenates two `ListT`s

> (<|>) :: ListT IO a -> ListT IO a -> ListT IO a

    * (`>>=`), which powers @do@ notation and @MonadComprehensions@

> (>>=) :: ListT IO a -> (a -> ListT IO b) -> ListT IO b

    * `select`, which converts a plain list into a `ListT`

> select :: [a] -> ListT IO a

-}

{- $monad

    Sometimes we can simplify the code by taking advantage of the fact that the
    `Monad` instance for `ListT` behaves like a list comprehension:

> stdout :: ListT IO String -> IO ()
> stdout strings = runListT (do
>     string <- strings
>     liftIO (putStrLn string) )

    You can read the above code as saying: \"for each @string@ in @strings@,
    call `putStrLn` on @string@."

    You can even use list comprehension syntax if you enable the
    @MonadComprehensions@ language extension:

> stdout strings = runListT [ r | str <- strings, r <- liftIO (putStrLn str) ]

    There are a few ways we could consider defining a `ListT` analogue to the `mapM`
    function from `Prelude`, but none are given in this library because they need
    require only (`>>=`) and some trivial lifting.

> mapM                                :: (a -> IO b)       -> [a]        -> IO [b]
> ( \f xs -> xs        >>=        f ) :: (a -> ListT IO b) -> ListT IO a -> ListT IO b
> ( \f xs -> select xs >>= lift . f ) :: (a -> IO b)       -> [a]        -> ListT IO b
> ( \f xs -> xs        >>= lift . f ) :: (a -> IO b)       -> ListT IO a -> ListT IO b

    A critical difference between `mapM` and `ListT`'s monad is that `ListT` will
    stream in constant space, whereas `mapM` buffers the entire output list before
    returning a single element.

-}

{- $interaction

    To test your understanding, guess what this code does and then test your
    guess by running the code:

@
import List.Transformer ('ListT', 'runListT', 'liftIO', ('<|>'), 'select')
import Data.Foldable ('Data.Foldable.asum')
import Data.List ('Data.List.repeat')

strings :: 'ListT' IO String
strings = do
    'select' ('Data.List.repeat' ())
    'Data.Foldable.asum'
        [ pure ""
        , pure "Say something:"
        , do
            x <- 'liftIO' getLine
            return ("You said: " '<|>' x)
        ]

main :: IO ()
main = 'runListT' (do
    string \<- pure "Hello, there!" '<|>' strings
    'liftIO' (putStrLn string) )
@

-}

{-| This is like a list except that you can interleave effects between each list
    element.  For example:

> stdin :: ListT IO String
> stdin = ListT (do
>     eof <- System.IO.isEOF
>     if eof
>         then return Nil
>         else do
>             line <- getLine
>             return (Cons line stdin) )

    The mnemonic is \"List Transformer\" because this type takes a base `Monad`,
    @\'m\'@, and returns a new transformed `Monad` that adds support for
    list comprehensions
-}
newtype ListT m a = ListT { next :: m (Step m a) }
    deriving (Foldable, Traversable)

instance MonadTrans ListT where
    lift m = ListT (do
        x <- m
        return (Cons x empty) )

instance Monad m => Functor (ListT m) where
    fmap k (ListT m) = ListT (do
        s <- m
        return (fmap k s) )

instance Monad m => Applicative (ListT m) where
    pure x = ListT (return (Cons x empty))

    ListT m <*> l = ListT (do
        s <- m
        case s of
            Nil       -> return Nil
            Cons f l' -> next (fmap f l <|> (l' <*> l)) )

    ListT m *> l = ListT (do
        s <- m
        case s of
            Nil       -> return Nil
            Cons _ l' -> next (l <|> (l' *> l)) )

    ListT m <* l = ListT (do
        s <- m
        case s of
            Nil       -> return Nil
            Cons x l' -> next ((x <$ l) <|> (l' <* l)) )

instance Monad m => Monad (ListT m) where
    return = pure

    ListT m >>= k = ListT (do
        s <- m
        case s of
            Nil       -> return Nil
            Cons x l' -> next (k x <|> (l' >>= k)) )

#if !(MIN_VERSION_base(4,13,0))
    fail _ = mzero
#endif

instance Monad m => Alternative (ListT m) where
    empty = ListT (return Nil)

    ListT m <|> l = ListT (do
        s <- m
        case s of
            Nil       -> next l
            Cons x l' -> return (Cons x (l' <|> l)) )

instance Monad m => MonadPlus (ListT m) where
    mzero = empty

    mplus = (<|>)

#if MIN_VERSION_base(4,9,0)
instance Monad m => MonadFail (ListT m) where
    fail _ = mzero
#endif

instance (Monad m, Data.Semigroup.Semigroup a) => Data.Semigroup.Semigroup (ListT m a) where
    (<>) = liftA2 (<>)

instance (Monad m, Data.Semigroup.Semigroup a, Monoid a) => Monoid (ListT m a) where
    mempty  = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance MonadIO m => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)

instance MonadError e m => MonadError e (ListT m) where
    throwError e = ListT (throwError e)

    catchError (ListT m) k = ListT (catchError m (next . k))

instance MonadReader i m => MonadReader i (ListT m) where
    ask = lift ask

    local k (ListT m) = ListT (do
        s <- local k m
        case s of
            Nil      -> return Nil
            Cons x l -> return (Cons x (local k l)) )

    reader k = lift (reader k)

instance MonadState s m => MonadState s (ListT m) where
    get = lift get

    put x = lift (put x)

    state k = lift (state k)

instance MFunctor ListT where
    hoist f xs = ListT (f (fmap (hoist f) (next xs)))

instance (Monad m, Num a) => Num (ListT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ListT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ListT m a) where
    pi = pure pi

    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

{-| Use this to drain a `ListT`, running it to completion and discarding all
    values.  For example:

> stdout :: ListT IO String -> IO ()
> stdout l = runListT (do
>     str <- l
>     liftIO (putStrLn str) )

    The most common specialized type for `runListT` will be:

> runListT :: ListT IO a -> IO ()
-}
runListT :: Monad m => ListT m a -> m ()
runListT (ListT m) = do
    s <- m
    case s of
        Nil       -> return ()
        Cons _ l' -> runListT l'

{-| Use this to fold a `ListT` into a single value.  This is designed to be
    used with the @foldl@ library:

> import Control.Foldl (purely)
> import List.Transformer (fold)
>
> purely fold :: Monad m => Fold a b -> ListT m a -> m b

    ... but you can also use the `fold` function directly:

> fold (+) 0 id :: Num a => ListT m a -> m a

>>> fold (<>) "" id (select ["a", "b", "c", "d", "e"])
"abcde"
-}
fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> ListT m a -> m b
fold step begin done l = go begin l
  where
    go !x (ListT m) = do
        s <- m
        case s of
            Cons a l' -> go (step x a) l'
            Nil       -> return (done x)

{-| Use this to fold a `ListT` into a single value.  This is designed to be
    used with the @foldl@ library:

> import Control.Foldl (impurely)
> import List.Transformer (fold)
>
> impurely fold :: Monad m => FoldM m a b -> ListT m a -> m b

    ... but you can also use the `foldM` function directly.
-}
foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ListT m a -> m b
foldM step begin done l0 = do
    x0 <- begin
    go x0 l0
  where
    go !x (ListT m) = do
        s <- m
        case s of
            Cons a l' -> do
                x' <- step x a
                go x' l'
            Nil       -> done x

{- $pleaseStream

    This library is designed to stream results in constant space and does not
    expose an obvious way to collect all the results into memory.  As a rule of
    thumb if you think you need to collect all the results in memory try to
    instead see if you can consume the results as they are being generated (such
    as in all the above examples).  If you can stream the data from start to
    finish then your code will use significantly less memory and your program
    will become more responsive.

-}

{- $constructing

    `empty` is the empty list with no effects.

    Use `pure`/`return` to construct a singleton list with no effects. Use `liftIO`
    to turn an effect into a singleton list whose sole element is the effect's result.

    Suppose you want to build a `ListT` with three elements and no effects.
    You could write:

> pure 1 <|> pure 2 <|> pure 3 :: ListT IO Int

    ... although you would probably prefer to use `select` instead:

> select [1, 2, 3] :: ListT IO Int

-}

{-| Convert any collection that implements `Foldable` to another collection that
    implements `Alternative`

    For this library, the most common specialized type for `select` will be:

> select :: [a] -> ListT IO a
-}
select :: (Foldable f, Alternative m) => f a -> m a
select = Data.Foldable.foldr cons empty
  where
    cons x xs = pure x <|> xs


-- | @take n xs@ takes @n@ elements from the head of @xs@.
--
-- >>> let list xs = do x <- select xs; liftIO (print (show x)); return x
-- >>> let sum = fold (+) 0 id
-- >>> sum (take 2 (list [5,4,3,2,1]))
-- "5"
-- "4"
-- 9
take :: Monad m => Int -> ListT m a -> ListT m a
take n l
    | n <= 0    = empty
    | otherwise = ListT (do
        s <- next l
        case s of
            Cons a l' -> return (Cons a (take (n-1) l'))
            Nil       -> return Nil)

-- | @drop n xs@ drops @n@ elements from the head of @xs@, but still runs their
-- effects.
--
-- >>> let list xs = do x <- select xs; liftIO (print (show x)); return x
-- >>> let sum = fold (+) 0 id
-- >>> sum (drop 2 (list [5,4,3,2,1]))
-- "5"
-- "4"
-- "3"
-- "2"
-- "1"
-- 6
drop :: Monad m => Int -> ListT m a -> ListT m a
drop n l
    | n <= 0    = l
    | otherwise = ListT (do
        s <- next l
        case s of
            Cons _ l' -> next (drop (n-1) l')
            Nil       -> return Nil)

-- | @dropWhile pred xs@ drops elements from the head of @xs@ if they
-- satisfy the predicate, but still runs their effects.
--
-- >>> let list xs = do x <- select xs; liftIO (print (show x)); return x
-- >>> let sum = fold (+) 0 id
-- >>> sum (dropWhile even (list [2,4,5,7,8]))
-- "2"
-- "4"
-- "5"
-- "7"
-- "8"
-- 20
dropWhile :: Monad m => (a -> Bool) -> ListT m a -> ListT m a
dropWhile pred l = ListT (do
    n <- next l
    case n of
        Cons x l'
            | pred x    -> next (dropWhile pred l')
            | otherwise -> return (Cons x l')
        Nil             -> return Nil )

-- | @takeWhile pred xs@ takes elements from @xs@ until the predicate @pred@ fails
--
-- >>> let list xs = do x <- select xs; liftIO (print (show x)); return x
-- >>> let sum = fold (+) 0 id
-- >>> sum (takeWhile even (list [2,4,5,7,8]))
-- "2"
-- "4"
-- "5"
-- 6
takeWhile :: Monad m => (a -> Bool) -> ListT m a -> ListT m a
takeWhile pred l = ListT (do
    n <- next l
    case n of
        Cons x l' | pred x -> return (Cons x (takeWhile pred l'))
        _                  -> return Nil )

{- $filter

To filter elements from a list based on a predicate, use `Control.Monad.guard`.
For example, the following function is analogous to `Data.List.filter`:

> filter :: Monad m => (a -> m Bool) -> ListT m a -> ListT m a
> filter pred as = do
>     a <- as
>     b <- lift (pred a)
>     guard b
>     return a

-}

{- $concatenation

    Use (`<|>`) to concatenate two lists.

    > (<|>) :: ListT IO a -> ListT IO a -> ListT IO a

    Use `Data.Foldable.asum` to flatten a list of lists.

    > asum :: [ListT IO a] -> ListT IO a

    Use `Control.Monad.join` to flatten a `ListT` of `ListT`s.

    > join :: ListT IO (ListT IO a) -> ListT IO a

-}

{- $pairwise

    The (`<>`) operation joins every combination of an element from one list with
    an element from the other.

>>> runListT ( (select ["a", "b"] <> select ["1", "2", "3"]) >>= (liftIO . print) )
"a1"
"a2"
"a3"
"b1"
"b2"
"b3"

    This is the same combinatorial effect that (`>>=`) produces.

>>> runListT (do x <- select ["a", "b"]; y <- select ["1", "2", "3"]; liftIO (print (x <> y)))
"a1"
"a2"
"a3"
"b1"
"b2"
"b3"

-}

{- $repetition

Unbounded repetition can be induced using @'select' ('Data.List.repeat' ())@.
For example, here are several functions analogous to 'Data.List.cycle':

> cycle1 :: Monad m => a -> ListT m a
> cycle1 a = do
>     select (Data.List.repeat ())
>     return a

> cycle2 :: Monad m => [a] -> ListT m a
> cycle2 as = do
>     select (Data.List.repeat ())
>     select as

> cycle3 :: Monad m => m a -> ListT m a
> cycle3 m = do
>     select (Data.List.repeat ())
>     lift m

> cycle4 :: Monad m => [m a] -> ListT m a
> cycle4 ms = do
>     select (Data.List.repeat ())
>     m <- select ms
>     lift m

> cycle5 :: Monad m => ListT m a -> ListT m a
> cycle5 x = do
>     select (Data.List.repeat ())
>     x

> cycle6 :: Monad m => [ListT m a] -> ListT m a
> cycle6 lists = do
>     select (Data.List.repeat ())
>     x <- select lists
>     x

In a similar manner, we can use 'Data.List.replicate' as the initial selection
to achieve bounded repetition:

> replicate1 :: Monad m => Int -> a -> ListT m a
> replicate1 n a = do
>     select (Data.List.replicate n ())
>     return a

> replicate2 :: Monad m => Int -> [a] -> ListT m a
> replicate2 n as = do
>     select (Data.List.replicate n ())
>     select as

> replicate3 :: Monad m => Int -> m a -> ListT m a
> replicate3 n m = do
>     select (Data.List.replicate n ())
>     lift m

> replicate4 :: Monad m => Int -> [m a] -> ListT m a
> replicate4 n ms = do
>     select (Data.List.replicate n ())
>     m <- select ms
>     lift m

> replicate5 :: Monad m => Int -> ListT m a -> ListT m a
> replicate5 n x = do
>     select (Data.List.replicate n ())
>     x

> replicate6 :: Monad m => Int -> [ListT m a] -> ListT m a
> replicate6 n lists = do
>     select (Data.List.replicate n ())
>     x <- select lists
>     x

-}

-- | @unfold step seed@ generates a 'ListT' from a @step@ function and an
-- initial @seed@.
unfold :: Monad m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfold step = loop
  where
    loop seed = ListT (do
        mx <- step seed
        case mx of
            Just (x, seed') -> return (Cons x (loop seed'))
            Nothing         -> return Nil)

-- | @zip xs ys@ zips two 'ListT' together, running the effects of each before
-- possibly recursing. Notice in the example below, @4@ is output even though
-- it has no corresponding element in the second list.
--
-- >>> let list xs = do x <- select xs; liftIO (print (show x)); return x
-- >>> runListT (zip (list [1,2,3,4,5]) (list [6,7,8]))
-- "1"
-- "6"
-- "2"
-- "7"
-- "3"
-- "8"
-- "4"
zip :: Monad m => ListT m a -> ListT m b -> ListT m (a, b)
zip xs ys = ListT (do
    sx <- next xs
    sy <- next ys
    case (sx, sy) of
        (Cons x xs', Cons y ys') -> return (Cons (x, y) (zip xs' ys'))
        _                        -> return Nil)


{-| Pattern match on this type when you loop explicitly over a `ListT` using
    `next`.  For example:

> stdout :: ListT IO String -> IO ()
> stdout l = do
>     s <- next l
>     case s of
>         Nil       -> return ()
>         Cons x l' -> do
>             putStrLn x
>             stdout l'
-}
data Step m a = Cons a (ListT m a) | Nil
    deriving (Foldable, Traversable)

instance Monad m => Functor (Step m) where
    fmap _  Nil       = Nil
    fmap k (Cons x l) = Cons (k x) (fmap k l)

instance MFunctor Step where
    hoist _ Nil         = Nil
    hoist f (Cons x xs) = Cons x (hoist f xs)

-- | Similar to 'ZipList' in /base/: a newtype wrapper over 'ListT' that
-- overrides its normal 'Applicative' instance (combine every combination)
-- with one that "zips" outputs together one at a time.
--
-- >>> let xs = do x <- select [1,2,3,4]; liftIO (print x)
-- >>> let ys = do y <- select [5,6]; liftIO (print y)
-- >>> runListT (xs *> ys)
-- 1
-- 5
-- 6
-- 2
-- 5
-- 6
-- 3
-- 5
-- 6
-- 4
-- 5
-- 6
-- >>> runListT (getZipListT (ZipListT xs *> ZipListT ys))
-- 1
-- 5
-- 2
-- 6
-- 3
--
-- Note that the final "3" is printed even though it isn't paired with
-- anything.
--
-- While this can be used to do zipping, it is usually more convenient to
-- just use 'zip'.  This is more useful if you are working with a function
-- that expects "an Applicative instance", written to be polymorphic over
-- all Applicatives.
newtype ZipListT m a = ZipListT { getZipListT :: ListT m a }
  deriving (Functor, Alternative, Foldable, Traversable, MonadTrans, Floating, Fractional, Num, Semigroup, Monoid)

instance Monad m => Applicative (ZipListT m) where
    pure x = ZipListT go
      where
        go = ListT (pure (Cons x go))
    ZipListT fs <*> ZipListT xs = ZipListT (fmap (uncurry ($)) (zip fs xs))
