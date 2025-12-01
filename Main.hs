import System.IO
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Ord (comparing)

splitOn :: Char -> String -> [String] 
splitOn _ [] = [] 
splitOn d s = 
  let (x, rest) = break (== d) s 
  in case rest of 
       []     -> [x] 
       (_:xs) -> x : splitOn d xs 

data Booking = Booking 
  { originCountry      :: String
  , destinationCountry :: String
  , peopleCount        :: Int      
  , hotelName          :: String
  , bookingPrice       :: Float
  , discount           :: Float    
  , profitMargin       :: Float
  } deriving (Show, Eq)


-- Safe parsers
readFloat :: String -> Float
readFloat s = case reads s of [(x, "")] -> x; _ -> 0.0

readInt :: String -> Int
readInt s = case reads s of [(x, "")] -> x; _ -> 0

parsePercent :: String -> Float
parsePercent s = 
    let cleanS = [c | c <- s, c /= '%']
    in readFloat cleanS / 100.0

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


-- Q3: Most Profitable Hotel
-- Criteria: Highest Profit Margin. If tied, Highest Visitor Count.
solveQ3 :: [Booking] -> String
solveQ3 bookings =
    let 
        -- Key: Unique Hotel (Name + Country)
        keyFn b = (hotelName b, destinationCountry b)

        -- Aggregator
        -- Returns a tuple: (Max Margin, Total Visitors)
        stats grp = 
            ( maximum [profitMargin b | b <- grp] -- Primary sorting key
            , sum     [peopleCount b  | b <- grp] -- Tie-breaker key
            )
        
        -- Map aggregation
        hotelStats = aggregateBy keyFn stats bookings

        -- compares 1st element (Margin) -> if equal, compares 2nd element (Visitors)
        ((name, country), (margin, visitors)) = maximumBy (comparing snd) hotelStats

    in unlines
        [ "3. Most Profitable Hotel (Profit Margin & Number of Visitors):"
        , "   Hotel:    " ++ name ++ " (" ++ country ++ ")"
        , "   Margin:   " ++ show margin
        , "   Visitors: " ++ show visitors
        ]


main :: IO ()
main = do
    content <- readFile "Hotel_Dataset.csv"
    let rows = drop 1 (lines content)
    
    let bookings = [b | line <- rows, Just b <- [parseBooking line]]
    
    putStrLn "--- Analysis Results ---"
    putStrLn (solveQ3 bookings)
