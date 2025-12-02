module Main where

import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

data Booking = Booking
  { destinationCountry :: String
  , hotelName :: String
  , noOfPeople :: Int
  , bookingPrice :: Int
  , discount :: String
  , profitMargin :: Float
  } deriving (Show, Eq)


parseLine :: String -> Booking
parseLine line = 
    let fields = splitOn "," line
        destCountry = fields !! 9
        hotel = fields !! 16
        people = read (fields !! 11) :: Int
        price = read (fields !! 20) :: Int
        disc = fields !! 21
        profit = read (fields !! 23) :: Float
    in Booking destCountry hotel people price disc profit


readBookings :: FilePath -> IO [Booking]
readBookings path = do
    content <- readFile path
    let fileLines = lines content
        dataLines = tail fileLines
    return (map parseLine dataLines)


uniqueCountries :: [String] -> [String]
uniqueCountries [] = []
uniqueCountries (x:xs) = x : uniqueCountries [y | y <- xs, y /= x]


countByCountry :: [Booking] -> [(String, Int)]
countByCountry bookings = 
    let allDest = [destinationCountry b | b <- bookings]
        unique = nub allDest
    in [(country, length [b | b <- bookings, destinationCountry b == country])
        | country <- unique]


findMaxBookings :: [(String, Int)] -> (String, Int)
findMaxBookings = maximumBy (comparing snd)


main :: IO ()
main = do
    putStrLn "======================================"
    putStrLn "  Hotel Bookings Analysis System"
    putStrLn "======================================"
    
    putStrLn "\nLoading data from Hotel_Dataset.csv..."
    bookings <- readBookings "Hotel_Dataset.csv"
    putStrLn ("Loaded " ++ show (length bookings) ++ " bookings successfully!")
    
    let counts = countByCountry bookings
        (country, count) = findMaxBookings counts
    
    putStrLn "\n=== Question 1: Country with Highest Bookings ==="
    putStrLn ("Destination Country: " ++ country)
    putStrLn ("Total Bookings: " ++ show count)
    
    putStrLn "\n======================================"
    putStrLn "  Analysis Complete!"
    putStrLn "======================================"