module Main where

lazySum :: Num a => [a] -> a
lazySum = go 0
  where go :: Num a => a -> [a] -> a
        go acc [] = acc
        go acc (x : xs) = go (acc + x) xs

main :: IO ()
main = print (lazySum [1..1000000])

