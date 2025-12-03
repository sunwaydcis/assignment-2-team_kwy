module Main where

import System.IO
import Data.List (maximumBy, nub)
import Data.Ord (comparing)


-- Custom splitOn function to parse CSV lines without external dependencies.
splitOn :: Char -> String -> [String] 
splitOn _ [] = [] 
splitOn d s = 
  let (x, rest) = break (== d) s 
  in case rest of 
       []     -> [x] 
       (_:xs) -> x : splitOn d xs 

-- Safe parsers
readFloat :: String -> Float
readFloat s = case reads s of [(x, "")] -> x; _ -> 0.0

readInt :: String -> Int
readInt s = case reads s of [(x, "")] -> x; _ -> 0

parsePercent :: String -> Float
parsePercent s = 
    let cleanS = [c | c <- s, c /= '%']
    in readFloat cleanS / 100.0

-- Higher-order function to aggregate list items by a key and apply an aggregator function.
aggregateBy :: (Eq k) => (a -> k) -> ([a] -> v) -> [a] -> [(k, v)]
aggregateBy keySelector aggregator items =
    let
        insert acc x =
            let k = keySelector x
            in case lookup k acc of
                Just vs -> (k, x:vs) : filter ((/= k) . fst) acc
                Nothing -> (k, [x]) : acc
        grouped = foldl insert [] items
    in map (\(k, vs) -> (k, aggregator (reverse vs))) grouped


-- DATA TYPE definition
-- Unified Booking data structure with all fields needed for Q1 Q2 Q3.

data Booking = Booking 
  { originCountry      :: String
  , destinationCountry :: String
  , peopleCount        :: Int      
  , hotelName          :: String
  , bookingPrice       :: Float
  , discount           :: Float    
  , profitMargin       :: Float
  } deriving (Show, Eq)


-- Parses a single CSV line into a Maybe Booking, ensuring robustness.
parseBooking :: String -> Maybe Booking
parseBooking line =
    let fields = splitOn ',' line
    in if length fields > 23
       then Just Booking
            { originCountry      = fields !! 6
            , destinationCountry = fields !! 9
            , peopleCount        = readInt (fields !! 11)
            , hotelName          = fields !! 16
            , bookingPrice       = readFloat (fields !! 20)
            , discount           = parsePercent (fields !! 21)
            , profitMargin       = readFloat (fields !! 23)
            }
       else Nothing

-- Reads the dataset file and returns a list of successfully parsed bookings.
readBookings :: FilePath -> IO [Booking]
readBookings path = do
    content <- readFile path
    let rows = drop 1 (lines content)
    return [b | line <- rows, Just b <- [parseBooking line]]



-- QUESTION 1: COUNTRY WITH HIGHEST BOOKINGS
-- Groups bookings by destination country and counts the total number of bookings for each.
-- Returns tuple (Country, Count)
countByCountry :: [Booking] -> [(String, Int)]
countByCountry bookings = 
    let allDest = [destinationCountry b | b <- bookings]
        unique = nub allDest
    in [(country, length [b | b <- bookings, destinationCountry b == country])
        | country <- unique]

-- Finds the country-count pair with the highest booking count.
findMaxBookings :: [(String, Int)] -> (String, Int)
findMaxBookings = maximumBy (comparing snd)

-- Main solver for Question 1.
solveQ1 :: [Booking] -> (String, Int)
solveQ1 = findMaxBookings . countByCountry


-- QUESTION 2: Most Economical Hotel
-- Uses your effPrice logic to find the hotel with the lowest effective price.
data Booking2 = Booking2
  { hName2 :: String
  , price2 :: Float
  , disc2 :: Float
  , profitMargin2 :: Float
  } deriving (Show, Eq)

effPrice :: Booking2 -> Float
effPrice (Booking2 _ p d pm) = p * (1 - d) * pm

findLowestBooking :: [Booking2] -> Booking2
findLowestBooking [] = error "No bookings!"
findLowestBooking [b] = b
findLowestBooking (b:bs) =
    let minRest = findLowestBooking bs
    in if effPrice b < effPrice minRest then b else minRest

solveQ2 :: [Booking] -> String
solveQ2 bs =
    let bookings2 = [Booking2 (hotelName b) (bookingPrice b) (discount b) (profitMargin b) | b <- bs]
    in hName2 (findLowestBooking bookings2)


-- QUESTION 3: Most Profitable Hotel
-- Criteria: Highest Profit Margin. If tied, Highest Visitor Count.
solveQ3 :: [Booking] -> String
solveQ3 bookings =
    let 
        -- Key: Unique Hotel (Name + Country)
        keyFn b = (hotelName b, destinationCountry b)
        
        -- Aggregator function that calculates (Max Profit Margin, Total Visitors) for a hotel group.
        stats grp = 
            ( maximum [profitMargin b | b <- grp] -- Primary sorting key
            , sum     [peopleCount b  | b <- grp] -- Tie-breaker key
            )
        
        -- map aggregation
        hotelStats = aggregateBy keyFn stats bookings
        
        -- compares 1st element (Margin) -> if equal, compares 2nd element (Visitors)
        ((name, country), (margin, visitors)) = maximumBy (comparing snd) hotelStats

    in unlines
        [ "3. Most Profitable Hotel (Profit Margin & Number of Visitors):"
        , "   Hotel:    " ++ name ++ " (" ++ country ++ ")"
        , "   Margin:   " ++ show margin
        , "   Visitors: " ++ show (floor visitors :: Int)
        ]



-- MAIN EXECUTION
main :: IO ()
main = do
    putStrLn "======================================"
    putStrLn "  Hotel Bookings Analysis System"
    putStrLn "======================================"
    
    putStrLn "\nLoading data from Hotel_Dataset.csv..."
    bookings <- readBookings "Hotel_Dataset.csv"
    putStrLn ("Loaded " ++ show (length bookings) ++ " bookings successfully!")
    
    -- --- QUESTION 1 EXECUTION ---
    let (country, count) = solveQ1 bookings
    putStrLn "\n=== 1. Country with Highest Bookings ==="
    putStrLn ("Destination Country: " ++ country)
    putStrLn ("Hotel: " ++ solveQ2 bookings)
    
    -- --- QUESTION 2 not done yet ---
    putStrLn "\n=== 2. Most Economical Hotel Option ==="
    putStrLn "Hotel: " ++ solveQ2 bookings)"
    
    -- --- QUESTION 3 EXECUTION ---
    putStrLn "\n"
    putStrLn (solveQ3 bookings)
    
    putStrLn "\n======================================"
    putStrLn "  Analysis Complete!"
    putStrLn "======================================"
