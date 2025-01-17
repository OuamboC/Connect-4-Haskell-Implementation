-- Importing necessary functions for handling IO operations and character validation
-- hFlush is used to flush the output buffer, ensuring that the prompt is displayed immediately
-- isAlpha is used to check if characters in a string are alphabetic
import System.IO (hFlush, stdout)
import Data.Char (isAlpha)
import Data.List (intercalate) 


-- Step 1 : Define the board 
-- Define the Board type as a 2D list of characters
type Board = [[Char]]

--Define an empty cell
emptyCell :: Char
emptyCell  = ' '

-- Initialise the board with empty cells
-- Function: initBoard
-- Parameters : rows ( number of rows in the board ) , cols ( number of columns in the board)
initBoard :: Int -> Int -> Board
initBoard rows cols = replicate rows (replicate cols emptyCell)

-- Step 2 : Display the board 

-- Function: showBoard ( Function to display the board )
-- Parameters: board (the current game board to be displayed)
-- Action: Prints the board to the console with grid lines

-- showBoard takes the board as input and uses mapM_ (higher-order function) to apply a function to each row of the board.
-- mapM_ is used to sequence IO actions, and putStrLn prints each row.
showBoard :: Board -> IO ()
showBoard board = mapM_ putStrLn formattedBoard
  where
    -- Function to format a single cell
    formatCell :: Char -> String
    formatCell cell = " " ++ [cell] ++ " |"

    -- Function to format a single row by joining cells with " |
    formatRow :: [Char] -> String 
    formatRow row = concatMap formatCell row

    -- Create a list of formatted rows
    boardRows = [formatRow row | row <- board]

    -- Define a line of dashes based on the number of columns
    dashLine = replicate (length (formatRow (head board))) '-'
    -- Combine the rows and dashes
    formattedBoard = concatMap (\row -> [row, dashLine]) (init boardRows) ++ [last boardRows]

-- Step 3 : Check Valid Move 
-- Function to check if a move is valid 
-- Function : isValidMove  ( must return a boolean)
-- Parameters : board ( the current game board), row (row index of the move), col ( col index of the move)
-- Returns : True if the move is within bounds and the cell is empty , otherwise False
isValidMove :: Board -> Int -> Int -> Bool
isValidMove board row col = 
  (row >= 0 && row < length board) && 
  (col >= 0 && col < length (head board)) && 
  (board !! row !! col == emptyCell)

-- Step 4 : Place Token
-- Function to place a token on the board ( must return a new baord with the token placed by the user )
-- Function: placeToken 
-- Parameters : board (the current game board), row (row index to place the token ), col (col index to place the token), token (the player's token character)
-- Returns : A new Board with the token placed at the specified position
placeToken :: Board -> Int -> Int -> Char -> Board
placeToken board row col token = 
  take row board ++ 
  [take col (board !! row) ++ [token] ++ drop (col + 1) (board !! row)] ++ 
  drop (row + 1) board

-- Step 5 : Check for four same token on the same lines
-- Function to check if a line contains four of the same token 
-- Function : checkLine
-- Parameters : line (a list of cells in a row or column), token (the player's token character)
-- Returns : True if the line contains exactly four of the specified token, otherwise False
checkLine :: [Char] -> Char -> Bool
checkLine line token = length (filter (== token) line) == 4

-- Step 6 : Check for a Winner
-- Function to check if there is a winner
-- Function: checkWinner
-- Parameters : board ( the current game board) , row (row index of the last move ), col (col index of the last move ), token (the player's token character)
-- Returns: True if there is a winning line (horizontal or vertical) containing four of the specified token, otherwise False
checkWinner :: Board -> Int -> Int -> Char -> Bool
checkWinner board row col token = 
    let rowLine = board !! row
        colLine = [board !! r !! col | r <- [0..(length board -1)]]
    in checkLine rowLine token || checkLine colLine token

-- Step 7 : Function to check if  a string is alphabetic
-- Function : isValidPlayerName
-- Parameters : name (the string to be validated)
-- Returns : True if the string contains only alphabetic characters, otherwise False
isValidPlayerName :: String -> Bool 
isValidPlayerName name = all isAlpha name

-- Step 8 : Ask the names of the player
-- Function to get the player's name
-- Function: getPlayerName
-- Parameters: playerNumber (an identifier for the player, e.g., "Player 1" or "Player 2")
-- Returns: The player's name as an IO action
getPlayerName :: String -> IO String
getPlayerName playerNumber = do
  putStr $ "Enter " ++ playerNumber ++ "'s name:"
  hFlush stdout -- Flush the output buffer to ensure the prompt is displayed immediately
  name <- getLine
  if isValidPlayerName name
    then return name
    else do
        putStrLn "Invalid name.Please enter a valid name with only letters (no numbers or special characters)"
        getPlayerName playerNumber -- Recursively call getPlayerName until a valid name is entered



-- Step 9 : Game Loop

-- Main game loop
-- Function: gameLoop
-- Parameters: 
--   board (the current game board)
--   currentPlayer (the current player's token character, such as 'O' or 'C')
-- Action:
--   1. Displays the current board.
--   2. Prompts the current player to enter their move (row and column).
--   3. Reads the player's input and parses the row and column.
--   4. Checks if the move is valid:
--      - If valid, places the token on the board and checks for a winner.
--      - If a winner is found, displays the board and announces the winner.
--      - If no winner, recursively calls gameLoop with the updated board and the next player.
--      - If invalid, prompts the player to try again and recursively calls gameLoop.

gameLoop :: Board -> Char -> String -> String -> IO ()
gameLoop board currentPlayer player1Name player2Name = do
  -- Determine the current player's name based on the token
  let currentPlayerName = if currentPlayer == 'O' then player1Name else player2Name
  -- Display the current board
  showBoard board
  -- Prompt the current player to enter their move
  putStr $ currentPlayerName ++ ", enter your move (row col):"
  hFlush stdout -- Flush the output buffer to ensure the prompt is displayed immediately
  
  -- Read the player's input
  input <- getLine
  -- Parse the row and column from the input
  let [row, col] = map read (words input)
  -- Check if the move is valid
  if isValidMove board row col
    then do
      -- Place the token on the board
      let newBoard = placeToken board row col currentPlayer
      -- Check if the current player has won
      if checkWinner newBoard row col currentPlayer
        then do
          -- Display the updated board and announce the winner
          showBoard newBoard
          putStrLn $ currentPlayerName ++ " wins!"
        else
          -- Recursively call gameLoop with the updated board and the next player
          gameLoop newBoard (if currentPlayer == 'O' then 'C' else 'O') player1Name player2Name
    else do
      -- If the move is invalid, prompt the player to try again
      putStrLn "Invalid move. Try again."
      -- Recursively call gameLoop with the same board and current player
      gameLoop board currentPlayer player1Name player2Name

-- Step 10 : Start the game 
-- Main function to start the game
-- Function: main
-- Action:
--   1. Initialises the game board with 5 rows and 5 columns.
--   2. Starts the game loop with the initial board and the first player 'O'.

main :: IO ()
main = do
  -- Initialize the game board with 5 rows and 5 columns
  let board = initBoard 5 5
  -- Prompt Welcome message
  putStrLn $ "Welcome to Connect 4 (5x5 grid)" 
  -- Prompt instructions
  putStrLn $ "Instructions: The first player to align four of their tokens vertically or horizontally wins the game!"
  -- Get the names of the players
  player1Name <- getPlayerName "Player 1"
  player2Name <- getPlayerName "Player 2"
  -- Start the game loop with the initial board and the first player 'O'
  gameLoop board 'O' player1Name player2Name


  
  
    





