module Main where

import Data.Char
import System.Environment
import Data.List
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad.Primitive
import System.IO.Unsafe

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
