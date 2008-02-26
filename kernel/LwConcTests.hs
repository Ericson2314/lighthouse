module LwConcTests where

import qualified LwConc.Substrate as S
--import LwConc.ConcLib(readCounter)
import H.Monad(H, liftIO)

atomically = liftIO . S.atomically
newTLSKey = liftIO . S.newTLSKey
getTLS = S.getTLS
setTLS :: S.TLSKey a -> a -> H ()
setTLS k a = liftIO (S.setTLS k a)

testTLS :: (String -> H ()) -> H ()
testTLS putStrLn =
  do ka <- newTLSKey (Just (True, 42))
     kb <- newTLSKey (\(x,y,z) -> (x && y, z^3))
     (a,b) <- atomically $ do b <- getTLS kb
                              a <- getTLS ka
                              return (a,b)
     putStrLn (show a)
     setTLS ka Nothing
     a <- atomically $ getTLS ka
     putStrLn (show a)
     setTLS ka (Just (False, 42))
     a <- atomically $ getTLS ka
     putStrLn (show a)
     putStrLn (show (b (True, False, 2)))
     return ()

testCounter :: (String -> H ()) -> H ()
testCounter putStrLn =
  do --i <- atomically $ readCounter
     let i = 0
     putStrLn ("timerHandler has been called " ++ show i ++ " times")

