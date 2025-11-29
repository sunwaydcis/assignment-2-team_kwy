module Main where

-- Only store what we need for Question 1: Destination Country
data Booking = Booking
  { destinationCountry :: String
  } deriving (Show, Eq)








main :: IO ()
main = putStrLn "Hello, Haskell!"
