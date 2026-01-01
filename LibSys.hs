-- FINAL PROJECT: Functional Library Management System 
-- AUTHOR: Lee How Seet (23057243)

module Main where
import System.IO
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)
import Data.List (tails)
import Control.Applicative (liftA2) 
import Control.Monad ((>=>))        
import Data.Semigroup ((<>))        
import Data.Monoid (mconcat)        

--DATA TYPES 
data Status = Available 
            | Borrowed String 
            deriving (Show, Read, Eq)

data Book = Book {
    bookId :: Int,
    title  :: String,
    author :: String,
    status :: Status
} deriving (Show, Read, Eq)

-- We use (<>) to combine book descriptions
class Describable a where
    describe :: a -> String

instance Describable Book where
    describe b = 
        "[" <> show (bookId b) <> "] " <> title b <> " by " <> author b <> " -- " <> describeStatus (status b)

describeStatus :: Status -> String
describeStatus Available = "Available"
describeStatus (Borrowed name) = "Borrowed by " <> name

-- using mconcat to show the menu text
menuText :: String
menuText = mconcat 
    [ "\n=== SUNWAY LIBRARY SYSTEM ===\n"
    , "1. View All Books\n"
    , "2. Search Books\n"
    , "3. Add New Book\n"
    , "4. Borrow Book\n"
    , "5. Return Book\n"
    , "6. Save & Exit\n"
    , "Select option: "
    ]

-- Add bood function using Functor
addBook :: [Book] -> String -> String -> [Book]
addBook library newTitle newAuthor = 
    let newId = if null library then 1 else bookId (last library) + 1
        newBook = Book { bookId = newId, title = newTitle, author = newAuthor, status = Available }
    in library ++ [newBook]

-- Using map to update book status
updateBookStatus :: Int -> Status -> [Book] -> [Book]
updateBookStatus targetId newStatus = map updateHelper
  where
    updateHelper b
        | bookId b == targetId = b { status = newStatus }
        | otherwise            = b

-- Using fmap (via <$>) implies applying the search filter over the structure.
searchBooks :: String -> [Book] -> [Book]
searchBooks query = filter (\b -> query `isInfixOf` title b || query `isInfixOf` author b)
  where
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

findBookById :: Int -> [Book] -> Maybe Book
findBookById _ [] = Nothing
findBookById tid (x:xs)
    | bookId x == tid = Just x
    | otherwise       = findBookById tid xs

-- Fine Calculation
calculateFine :: Int -> Double
calculateFine days
    | days <= 0 = 0.0
    | days <= 3 = 0.50 + calculateFine (days - 1)
    | otherwise = 1.00 + calculateFine (days - 1)

-- Monad Kleisli Composition
-- Creating a reusable pipeline: Prompt -> Get Input -> Trim/Process
ask :: String -> IO String
ask prompt = putStr prompt >> hFlush stdout >> getLine

--  Using >=> to compose IO actions
-- This creates a function that takes a String (prompt), gets input, and validates it.
askAndValidate :: String -> IO (Maybe Int)
askAndValidate = ask >=> (\input -> return (readMaybe input))

-- IO & USER INTERFACE
dbFileName :: String
dbFileName = "library_db.txt"
-- generated a set of 50 entries for testing using AI

saveDB :: [Book] -> IO ()
saveDB lib = writeFile dbFileName (show lib)

loadDB_Safe :: IO [Book]
loadDB_Safe = 
    tryIO (readFile dbFileName) >>= \result ->
    case result of
        Left _ -> return [] 
        Right contents -> 
            case readMaybe contents of
                Just lib -> return lib
                Nothing -> return []

tryIO :: IO a -> IO (Either IOError a)
tryIO action = catchIOError (fmap Right action) (\e -> return (Left e))

-- run Menu function in infinite loop until exit
runMenu :: [Book] -> IO ()
runMenu library = do
    putStr menuText 
    choice <- getLine
    
    case choice of
        "1" -> do
            putStrLn "\n[Inventory List]"
            mapM_ putStrLn (describe <$> library) 
            runMenu library

        "2" -> do
            query <- ask "Enter search term: "
            let results = searchBooks query library
            if null results 
                then putStrLn "No books found." 
                else mapM_ putStrLn (describe <$> results)
            runMenu library

        "3" -> do
            title <- ask "Enter Title: "
            author <- ask "Enter Author: "
            let newLib = addBook library title author
            putStrLn "Book added successfully!"
            runMenu newLib

        "4" -> do
            mId <- askAndValidate "Enter Book ID to borrow: "
            case mId of
                Nothing -> putStrLn "Invalid ID." >> runMenu library
                Just tid -> 
                    case findBookById tid library of
                        Nothing -> putStrLn "Book not found." >> runMenu library
                        Just book -> 
                            if status book /= Available
                                then putStrLn "Book already borrowed." >> runMenu library
                                else do
                                    name <- ask "Enter Borrower Name: "
                                    let newLib = updateBookStatus tid (Borrowed name) library
                                    putStrLn ("Book borrowed by " <> name)
                                    runMenu newLib

        "5" -> do
            --using LiftA2 to combine two imputs to change book status and calculate fine
            
            mId <- askAndValidate "Enter Book ID to return: "
            mDays <- askAndValidate "Enter Overdue Days: "
            
            -- Using Applicative to check if BOTH inputs are valid (Just)
            -- liftA2 (,) (Just 1) (Just 5) = Just (1, 5)
            -- If either is Nothing, the result is Nothing.
            case liftA2 (,) mId mDays of
                Nothing -> putStrLn "Invalid Input (ID or Days)." >> runMenu library
                Just (tid, days) -> do
                     case findBookById tid library of
                        Nothing -> putStrLn "Book not found." >> runMenu library
                        Just book -> 
                            case status book of
                                Available -> putStrLn "Book was not borrowed." >> runMenu library
                                Borrowed _ -> do
                                    let fine = calculateFine days
                                    putStrLn $ "Returned. Fine: RM " <> show fine
                                    runMenu (updateBookStatus tid Available library)

        "6" -> saveDB library >> putStrLn "Saved. Goodbye!"
            
        _ -> putStrLn "Invalid selection." >> runMenu library

main :: IO ()
main = loadDB_Safe >>= runMenu