{-# LANGUAGE DeriveDataTypeable #-}
import Prelude hiding (catch)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Exception.Peel
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Peel (MonadPeelIO)
import Data.IORef
import Data.Maybe
import Data.Typeable (Typeable)

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.RWS as RWS

main :: IO ()
main = defaultMain
    [ testSuite "IdentityT" runIdentityT
    , testSuite "MaybeT" $ fmap fromJust . runMaybeT
    , testSuite "ReaderT" $ flip runReaderT "reader state"
    , testSuite "WriterT" runWriterT'
    , testSuite "StateT" $ flip evalStateT "state state"
    , testSuite "RWST" $ \m -> runRWST' m "RWS in" "RWS state"
    , testCase "WriterT tell" case_tell
    ]
  where
    runWriterT' :: Functor m => WriterT [Int] m a -> m a
    runWriterT' = fmap fst . runWriterT
    runRWST' :: (Monad m, Functor m) => RWS.RWST r [Int] s m a -> r -> s -> m a
    runRWST' m r s = fmap fst $ RWS.evalRWST m r s

testSuite :: MonadPeelIO m => String -> (m () -> IO ()) -> Test
testSuite s run = testGroup s
    [ testCase "finally" $ case_finally run
    , testCase "catch" $ case_catch run
    , testCase "bracket" $ case_bracket run
    , testCase "bracket_" $ case_bracket_ run
    , testCase "onException" $ case_onException run
    ]

ignore :: IO () -> IO ()
ignore x =
    catch x go
  where
    go :: SomeException -> IO ()
    go _ = return ()

data Exc = Exc
    deriving (Show, Typeable)
instance Exception Exc

one :: Int
one = 1

case_finally :: MonadPeelIO m => (m () -> IO ()) -> Assertion
case_finally run = do
    i <- newIORef one
    ignore
        (run $ (do
            liftIO $ writeIORef i 2
            error "error") `finally` (liftIO $ writeIORef i 3))
    j <- readIORef i
    j @?= 3

case_catch :: MonadPeelIO m => (m () -> IO ()) -> Assertion
case_catch run = do
    i <- newIORef one
    run $ (do
        liftIO $ writeIORef i 2
        throw Exc) `catch` (\Exc -> liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3

case_bracket :: MonadPeelIO m => (m () -> IO ()) -> Assertion
case_bracket run = do
    i <- newIORef one
    ignore $ run $ bracket
        (liftIO $ writeIORef i 2)
        (\() -> liftIO $ writeIORef i 4)
        (\() -> liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_bracket_ :: MonadPeelIO m => (m () -> IO ()) -> Assertion
case_bracket_ run = do
    i <- newIORef one
    ignore $ run $ bracket_
        (liftIO $ writeIORef i 2)
        (liftIO $ writeIORef i 4)
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_onException :: MonadPeelIO m => (m () -> IO ()) -> Assertion
case_onException run = do
    i <- newIORef one
    ignore $ run $ onException
        (liftIO (writeIORef i 2) >> error "ignored")
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3
    ignore $ run $ onException
        (liftIO $ writeIORef i 4)
        (liftIO $ writeIORef i 5)
    k <- readIORef i
    k @?= 4

case_tell :: Assertion
case_tell = do
    i <- newIORef one
    ((), w) <- runWriterT $ bracket_
        (liftIO (writeIORef i 2) >> tell [1])
        (liftIO (writeIORef i 4) >> tell [3])
        (liftIO (writeIORef i 3) >> tell [2])
    j <- readIORef i
    j @?= 4
    w @?= [2]

    ((), w') <- runWriterT $ bracket
        (liftIO (writeIORef i 5) >> tell [5])
        (const $ liftIO (writeIORef i 7) >> tell [7])
        (const $ liftIO (writeIORef i 6) >> tell [6])
    j' <- readIORef i
    j' @?= 7
    w' @?= [5, 6]
