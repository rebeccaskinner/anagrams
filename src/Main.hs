module Main where

import Data.Char
import System.Environment
import Data.List
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed (freeze)
import Control.Monad.Primitive
import System.IO.Unsafe
import Control.Monad

type Anagramer = String -> String -> Bool

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0 ]

alphabetIndex :: Char -> Int
alphabetIndex c = ord c - ord 'A'

toPrime :: Char -> Integer
toPrime = (primes !!) . alphabetIndex

strProduct :: String -> Integer
strProduct = foldl (\prod thisLetter -> prod * toPrime thisLetter) 1

anagramPrime :: Anagramer
anagramPrime a b = strProduct a == strProduct b

anagramSort :: Anagramer
anagramSort a b = (sort a) == (sort b)

anagramVector :: Anagramer
anagramVector a b = unsafePerformIO $ do
  emptyVect <- newEmpty 26
  added <- foldM (updateVect (+1)) emptyVect a
  subed <- foldM (updateVect ((-)1)) added b
  final <- freeze subed
  expected <- freeze emptyVect
  return $ expected == final

newEmpty :: Int -> IO (V.MVector (PrimState IO) Int)
newEmpty size = do
  v <- V.new size
  V.set v 0
  return v

updateVect :: (Int -> Int) -> V.MVector (PrimState IO) Int -> Char -> IO (V.MVector (PrimState IO) Int)
updateVect f v c =
  let idx = alphabetIndex c in do
    oldVal <- V.read v idx
    V.write v idx (f oldVal)
    return v

anagrams :: Anagramer -> [String] -> Bool
anagrams _ [] = True
anagrams _ [_] = True
anagrams f (x:y:rest) = (f x y) && anagrams f (y:rest)

main :: IO ()
main = do
  (method : fileList) <- getArgs
  files <- mapM readFile fileList
  let filteredList = map (filter isLetter . map toLower) files
  let f = getMethod method
  print $ (`anagrams` filteredList) <$> f

getMethod :: String -> Either String Anagramer
getMethod m
 | m == "prime" = Right anagramPrime
 | m == "sort" = Right anagramSort
 | m == "vector" = Right anagramVector
 | otherwise = Left "unknown solver strategy"
