module Main where

import Data.Char
import System.Environment

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0 ]

alphabetIndex :: Char -> Int
alphabetIndex c = (ord . toUpper $ c) - a
  where a = ord 'A'

toPrime :: Char -> Int
toPrime = (primes !!) . alphabetIndex

strProduct :: String -> Int
strProduct = product . map toPrime . filter isLetter

anagram :: String -> String -> Bool
anagram a b = strProduct a == strProduct b

anagrams :: [String] -> Bool
anagrams [] = True
anagrams [_] = True
anagrams (x:y:rest) = (anagram x y) && anagrams (y:rest)

main :: IO ()
main = do
  args <- getArgs
  print $ anagrams args
