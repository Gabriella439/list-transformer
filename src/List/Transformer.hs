{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-| The `ListT` type is like a list that lets you interleave effects between
    each element of the list.  The type's definition is very short:

> -- Every `ListT` begins with an outermost effect (the `m`)
> newtype ListT m a = ListT { next :: m (Step m a) }
> 
>
> -- The return value of that effect is either
> -- * Cons: a new list element followed by the rest of the list
> -- * Nil : an empty list
> data Step m a = Cons a (ListT m a) | Nil

    You most commonly use this type when you wish to generate each element of
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

    Sometimes we can simplify the code by taking advantage of the fact that the
    `Monad` instance for `ListT` behaves like a list comprehension:

> stdout :: ListT IO String -> IO ()
> stdout strings = runListT (do
>     string <- strings
>     liftIO (putStrLn string) )

    You can read the above code as saying: \"for each @string@ in @strings@,
    call `putStrLn` on @string@.

    You can even use list comprehension syntax if you enable the
    @MonadComprehensions@ language extension:

> stdout strings = runListT [ r | string <- strings, r <- liftIO (putStrLn str) ]

    The most important operations that you should familiarize yourself with are:

    * `empty`, which gives you an empty `ListT` with 0 elements

> empty :: ListT IO a

    * `pure` / `return`, which both convert a value into a one-element `ListT`

> pure, return :: a -> ListT IO a

    * `liftIO`, which converts an `IO` action into a one-element `ListT`

> liftIO :: IO a -> ListT IO a

    * (`<|>`), which concatenates two `ListT`s

> (<|>) :: ListT IO a -> ListT IO a -> ListT IO a

    * (`>>=`), which powers @do@ notation and @MonadComprehensions@:

> (>>=) :: ListT IO a -> (a -> ListT IO b) -> ListT IO b

    For example, suppose you want to a build a `ListT` with three elements and
    no effects.  You could just write:

> pure 1 <|> pure 2 <|> pure 3 :: ListT IO Int

    ... although you would probably prefer to use `select` instead:

> select :: [a] -> ListT IO a
>
> select [1, 2, 3] :: ListT IO Int

    To test your understanding, guess what this code does and then test your
    guess by running the code:

> import List.Transformer
>
> strings :: ListT IO String
> strings = do
>     _ <- select (repeat ())
>     liftIO (putStrLn "Say something:")
>     liftIO getLine
>
> main :: IO ()
> main = runListT (do
>     string <- pure "Hello, there!" <|> strings
>     liftIO (putStrLn string) )

    This library does not provide utilities like `mapM` because there are many
    possible minor variations on `mapM` that we could write, such as:

> mapM :: Monad m => (a -> m b) -> [a] -> ListT m b
> mapM f xs = do
>     x <- select xs
>     lift (f x)
>
> -- Alternatively, using MonadComprehensions:
> mapM f x = [ r | x <- select xs, r <- lift (f x) ]

    ... or:

> mapM :: Monad m => (a -> m b) -> ListT m a -> ListT m b
> mapM f xs = do
>     x <- xs
>     lift (f x)
>
> -- Alternatively, using MonadComprehensions:
> mapM f x = [ r | x <- xs, r <- lift (f x) ]

    ... or:

> mapM :: Monad m => (a -> ListT m b) -> ListT m a -> ListT m b
> mapM f xs = do
>     x <- xs
>     f x
>
> -- Alternatively, using MonadComprehensions:
> mapM f x = [ r | x <- xs, r <- f x ]
>
> -- Alternatively, using a pre-existing operator from "Control.Monad"
> mapM = (=<<)

    Whichever one you prefer, all three variations still stream in constant
    space (unlike @"Control.Monad".`mapM`@, which buffers the entire output
    list before returning a single element).
-}
module List.Transformer
    ( -- * ListT
      ListT(..)
    , runListT
    , fold
    , foldM
    , select

      -- * Step
    , Step(..)

      -- * Re-exports
    , MonadTrans(..)
    , MonadIO(..)
    , Alternative(..)
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
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))

import qualified Data.Foldable

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

instance (Monad m, Monoid a) => Monoid (ListT m a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

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

{-| Convert any collection that implements `Foldable` to another collection that
    implements `Alternative`

    For this library, the most common specialized type for `select` will be:

> select :: [a] -> ListT IO a
-}
select :: (Foldable f, Alternative m) => f a -> m a
select = Data.Foldable.foldr cons empty
  where
    cons x xs = pure x <|> xs

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
