{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import ViperVM.Utils.ContFlow
import ViperVM.Utils.Monad
import Data.Char

-- | Explicit CPS
sample1 :: (Int -> IO r) -> (Float -> IO r) -> (String -> IO r) -> IO r
sample1 cint cfloat cstring = do
   putStrLn "Test"
   if (10 :: Int) > 20
      then cint 10
      else cstring "Pif"

-- | CPS into tuple
sample2 :: ( Int -> IO r, Float -> IO r, String -> IO r) -> IO r
sample2 (cint,  cfloat,  cstring) = do
   putStrLn "Test"
   if (10 :: Int) > 20
      then cint 10
      else cstring "Pif"

-- | CPS into tuple by explicit type
sample3 :: (Int -> IO r, Float -> IO r, String -> IO r) -> IO r
sample3 cs = do
   putStrLn "Test"
   if (10 :: Int) > 20
      then fret @Int cs 10
      else fret @String cs "Pif"

-- | CPS into tuple by implicit type
sample4 :: (Int -> IO r, Float -> IO r, String -> IO r) -> IO r
sample4 cs = do
   putStrLn "Test"
   if (10 :: Int) > 20
      then fret cs (10 :: Int)
      else fret cs "Pif"

-- | Generalize
sample5 :: (Int -> r, Float -> r, String -> r) -> r
sample5 cs =
   if (10 :: Int) > 20
      then fret cs (10 :: Int)
      else fret cs "Pif"

-- | Put in a generic tuple
sample6 :: ContMapR (Int,Float,String) r -> r
sample6 cs =
   if (10 :: Int) > 20
      then fret cs (10 :: Int)
      else fret cs "Pif"

-- | Wrap in a newtype
sample7 :: ContFlow (Int,Float,String) r
sample7 = ContFlow $ \cs -> 
   if (10 :: Int) > 20
      then fret cs (10 :: Int)
      else fret cs "Pif"

-- | Example of using a flow
sample8 :: IO Int
sample8 = sample7 >::>
   ( \(x :: Int)    -> putStrLn ("Int: " ++ show x) >> return 1
   , \(x :: Float)  -> putStrLn ("Float: " ++ show x) >> return 2
   , \(x :: String) -> putStrLn ("String: " ++ show x) >> return 3
   )

-- | Example of combined flows
sample9 :: ContFlow (Double,Char) (IO r)
sample9 = ContFlow $ \cs -> do
   putStrLn "Forcing an IO monad"
   sample7 >::>
      ( \(x :: Int)    -> fret cs 'a'
      , \(x :: Float)  -> fret cs (2.0 :: Double)
      , \(x :: String) -> fret cs 'b'
      )

sample9' :: ContFlow (Float,Char) (IO r)
sample9' = ContFlow $ \cs -> do
   putStrLn "Forcing an IO monad"
   sample7 >::>
      ( \(x :: Int)    -> fret cs 'a'
      , fret cs -- direct transfer of the result
      , \(x :: String) -> fret cs 'b'
      )

sample10 :: IO ()
sample10 = do
   putStrLn "Test test test!"
   sample9 >::>
      ( \(x :: Double) -> putStrLn ("Double: " ++ show x)
      , \(x :: Char)   -> putStrLn ("Char: " ++ show x)
      )

-- | What we would like to write (made up syntax)
-- sample11 :: ContFlow '[Double,Char] r
-- sample11 = cdo
--    ccase sample7 of
--       (x :: Int)    -> return 'a'
--       (x :: Float)  -> return (2.0 :: Double)
--       (x :: String) -> return 'b'
--

-- this define has to be defined in each module using ContFlow for now
#define fdo ContFlow $ \__cs -> let ?__cs = __cs in do

-- | Implicit parameters
sample12 :: MonadIO m => Int -> ContFlow (Double,Char) (m r)
sample12 n = fdo
   liftIO $ putStrLn "Forcing an IO monad"
   sample7 >::>
      ( \(x :: Int)    -> freturnN @1 'a' -- indexed return
      , \(x :: Float)  -> freturn (2.0 :: Double)
      , \(x :: String) -> if n < 10 
                              then frec (sample12 (n+1)) -- recursive call
                              else freturn (fromIntegral n :: Double)
      )

sample13 :: IO ()
sample13 = do
   putStrLn "Test test test!"
   sample12 0 >::>
      ( \(x :: Double) -> putStrLn ("Double: " ++ show x)
      , \(x :: Char)   -> putStrLn ("Char: " ++ show x)
      )

parseDigit :: String -> ContFlow ((Int,String), String, ()) r
parseDigit s = fdo
   case s of
      ""       -> freturn ()
      (x:xs) | o <- ord x, o >= ord '0' && o <= ord '9'
               -> freturn (o - ord '0',xs)
      _        -> freturn s

parseDigits :: String -> [Int]
parseDigits s = parseDigit s >::>
   ( \(x,xs) -> x : parseDigits xs
   , \(x:xs) -> parseDigits xs
   , \()     -> []
   )

parseNum :: forall r. String -> ContFlow ((Int,String), String, ()) r
parseNum str = fdo
   let
      go :: Bool -> Int -> String -> r
      go b i s = parseDigit s >::>
            ( \(x,xs) -> go True (i*10+x) xs
            , \xs     -> if b then freturn (i,xs) else freturn xs
            , \()     -> if b then freturn (i,"") else freturn ()
            )

   go False 0 str

data Token = TokenInt Int | TokenString String deriving (Show)

parseTokens :: String -> [Token]
parseTokens str = go str ""
   where
      go s lb = parseNum s >:~:> -- support wrong order
         ( \(x,xs) -> if lb /= "" then TokenString (reverse lb) : TokenInt x : go xs ""
                                  else TokenInt x : go xs ""
         , \()     -> if lb /= "" then [TokenString (reverse lb)] else []
         , \(x:xs) -> go xs (x:lb)
         )

data Then = Then
data Else = Else

iff :: Bool -> ContFlow (Then,Else) r
{-# INLINE iff #-}
iff b = fdo
   case b of
      True  -> freturn Then
      False -> freturn Else

testIf :: Bool -> IO ()
testIf b = do
   iff b >:~:>
      ( \Else -> putStrLn "No!"
      , \Then -> putStrLn "Yes!"
      )


main :: IO ()
main = do
   print (parseTokens "123adsf456sfds789")
