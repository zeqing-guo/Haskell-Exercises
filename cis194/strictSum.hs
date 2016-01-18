module Main where

strictSum :: Num a => [a] -> a
strictSum = go 0
  where go acc []     = acc
        go acc (x:xs) = acc `seq` go (x + acc) xs

main :: IO ()
main = print (strictSum [1..1000000])

