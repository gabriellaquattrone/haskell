-- Authors: Gabriella Quattrone and Jenhifer Salas

-- This program will make a move in a game of
-- capture the flag.
-- It takes a current state as a string, a list
-- of states, and turn marker as a character.
-- It returns the next state as a string

-- In general we need to do the following:
--      1. generate the possible moves depending on depth
--      2. return a list of moves in the order of best to worst
--      3. check if the first move of the list has been is in history
--          if True then check the next possible best move in the list (repeat step 3)
--          if False then return the move

-- Assume white at top of board always, black at bottom. Turns can still change.

-- The Imports
import Data.Fixed


-- The Interface
capture :: [String] -> Char -> Int -> String
capture history turn depth = head history    -- for now, change later
 -- pass in size somehow
 -- pass to minimax, which will call movegen

-- The Types
type Board = String -- a state of the board
type EBoard = (Board, Int) -- a state of the board and heuristic value
type EBoards = [EBoard]  -- a list of evaluated boards
type Coordinate = (Int, Int) -- the coordinates for each piece on the board


-- heuristic 1: having more or less pawns (more = closer to winning, less = further away from winning)
-- heuristic 2: how close the flag is to the other side (rectilinear)
-- heuristic 3: did we capture a flag?

numPcs :: Board -> Char -> Int
numPcs state turn = numPcsHelper state turn 0

numPcsHelper :: Board -> Char -> Int -> Int
numPcsHelper state 'w' count
    | null state            = count
    | (head state) == 'w'   = numPcsHelper (tail state) 'w' (count + 1)
    | (head state) == 'W'   = numPcsHelper (tail state) 'w' (count + 1)
    | otherwise             = numPcsHelper (tail state) 'w' count

numPcsHelper state 'b' count
    | null state            = count
    | (head state) == 'b'   = numPcsHelper (tail state) 'b' (count + 1)
    | (head state) == 'B'   = numPcsHelper (tail state) 'b' (count + 1)
    | otherwise             = numPcsHelper (tail state) 'b' count


-- Moves Generator
-- Assume size of board has already been sqrt'd to dim
-- moveGen :: Board -> Int -> EBoards
-- moveGen currState dim = moveGenHelper currState currState dim 0

-- moveGenHelper :: Board -> Board -> Int -> Int -> EBoards
-- moveGenHelper currState mutState dim pos
--     | null mutState          = []
--     | head mutState == 'w'   = (wMove currState 'w' size (rowCol pos dim)) : moveGenHelper currState (tail mutState) size (pos + 1)
--     | head mutState == 'b'   = (bMove currState 'b' size (rowCol pos dim)) : moveGenHelper currState (tail mutState) size (pos + 1)
--     | head mutState == 'W'   = (WMove currState 'W' size (rowCol pos dim)) : moveGenHelper currState (tail mutState) size (pos + 1)
--     | head mutState == 'B'   = (BMove currState 'B' size (rowCol pos dim)) : moveGenHelper currState (tail mutState) size (pos + 1)
--

-- wMove :: Board -> Int -> Int -> Board
-- wMove currState mutState dim pos

-- Returns the row and column of an indicated position
-- Assume the dimension has already been sqrt'd to dim
rowCol :: Int -> Int -> Coordinate
rowCol pos dim = ((div pos dim), (mod pos dim))

-- If mod pos dim == 0, it's in the first column
-- If mod pos dim == 1, it's in the second column
-- If mod pos dim == dim - 2, it's in the second to last column
-- If mod pos dim == dim - 1, it's in the last column
-- possPos :: Int -> Int -> Char -> [Int]
-- possPos pos dim "w"
--     | mod pos dim == 0        = [pos + 1, pos + 2, pos + dim, pos + ( 2 * dim ) ]
--     | mod pos dim == 1        = [pos - 1, pos + 1, pos + 2, pos + dim, pos + ( 2 * dim ) ]
--     | mod pos dim == dim - 2  = [pos - 2, pos - 1, pos + 1, pos + dim, pos + ( 2 * dim ) ]
--     | mod pos dim == dim - 1  = [pos - 2, pos - 1, pos + dim, pos + ( 2 * dim ) ]
--     | otherwise               = [pos - 2, pos - 1, pos + 1, pos + 2, pos + dim, pos + ( 2 * dim ) ]

-- possPos pos dim "b"
--     | mod pos dim == 0       = [pos + 1, pos + 2, pos - dim, pos - ( 2 * dim ) ]
--     | mod pos dim == 1        = [pos - 1, pos + 1, pos + 2, pos - dim, pos - ( 2 * dim ) ]
--     | mod pos dim == dim - 2  = [pos - 2, pos - 1, pos + 1, pos - dim, pos - ( 2 * dim ) ]
--     | mod pos dim == dim - 1        = [pos - 2, pos - 1, pos + dim, pos - ( 2 * dim ) ]
--     | otherwise               = [pos - 2, pos - 1, pos + 1, pos + 2, pos - dim, pos - ( 2 * dim ) ]

-- possPos pos dim "W"

-- possPos pos dim "B"
