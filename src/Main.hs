module Main where

import Data.Char
import System.Environment
import Data.List
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad.Primitive
import System.IO.Unsafe

type Anagramer = String -> String -> Bool

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0 ]

alphabetIndex :: Char -> Int
alphabetIndex c = ord c - ord 'A'

toPrime :: Char -> Int
toPrime = (primes !!) . alphabetIndex

strProduct :: String -> Int
strProduct = foldl (\prod thisLetter -> prod * toPrime thisLetter) 1

anagramPrime :: Anagramer
anagramPrime a b = strProduct a == strProduct b

anagramSort :: Anagramer
anagramSort a b = (sort a) == (sort b)

updateField v c = let idx = alphabetIndex c in do
  old <- MV.read v idx
  MV.write v idx (old + 1)
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

getMethod :: String -> Maybe Anagramer
getMethod m
 | m == "prime" = Just anagramPrime
 | m == "sort" = Just anagramSort
 | otherwise = Nothing
