-- Authors: Gabriella Quattrone and Jenhifer Salas

-- This program returns the best move in a game of
-- capture the flag given a certain depth, history, and turn.
-- It returns the next best move as a string containing the board
-- of all the pieces and their positions.

-- Our Strategy:
--      1. Generate the possible moves depending on depth
--      2. Return a list of moves in the order of best to worst
--      3. Check if the first move of the list has been is in history
--          if True then check the next possible best move in the list (repeat step 3)
--          if False then return the move

-- Assume white at top of board always, black at bottom. Turns can still change.
-- Heuristic 1: Comparing how many pawns one has versus the opponent (more = more likely to win, less = less likely to win)
-- Heuristic 2: How close the flag is to the other side (we use Rectilinear/Manhattan Distance to calculate this)

-- Ways of winning
-- 1: All opponents pawns are gone
-- 2: Flag has been captured
-- 3: All other moves are illegal moves for the opponent
-- 4: A flag has gotten to the other side unscathed

import Data.Char -- for toUpper and toLower

-- The Types
type Board = String -- a state of the board
type Boards = [Board] -- a list of non-evaluated boards
type EBoard = (Board, Int) -- a state of the board and heuristic value
type EBoards = [EBoard]  -- a list of evaluated boards
type Coordinate = (Int, Int) -- the coordinates for each piece on the board
type Move = (Coordinate, Char) -- the coordinates for specific pieces

-- OUR TESTER
-- Not part of the assignment, but a tester to check work.
start = "-wWw--www-------bbb--bBb-"
playgame :: Board -> Char ->  Int -> Boards
playgame start turn depth = playgamehelper start [] (opposite turn) depth

playgamehelper :: Board -> Boards -> Char -> Int -> Boards
playgamehelper state history turn depth
    | not (null history) && state == (head history) && null state
                    = state : history
    | null nextmove = state : history
    | otherwise     = playgamehelper nextmove (state:history) nextplayer depth
    where nextplayer = opposite turn
          nextmove   = capture (state:history) nextplayer depth

-- The Interface
-- Our top level function which gets called by the user to return the next best move.
-- Requires the history (a list of boards), turn (white or black), and depth (how deep we search).
capture :: Boards -> Char -> Int -> Board
capture (state:_) _ 0 = state
capture history turn depth
    | null nextmoves || over = []
    | otherwise = fst (head moves)
    where moves = maxSort (map (\ x -> mapMiniMax x x) nextmoves)
          state = head history
          size = length state
          dim = round (sqrt (fromIntegral size))
          status = evaluateBoard state turn dim
          over = status >= 100 || status == -100
          nextmoves = validMoves history (moveGen state dim size turn) size
          mapMiniMax = miniMax turn (opposite turn) (depth - 1) size dim history


-- Our miniMax algorithm. Requires a player, which is differentiated from turn as we
-- switch them depending on the depth level. The "size" of the board and "dim" (dimension)
-- are used to pass into evaluateBoard (which returns a list of evaluated boards)
-- and the "hist" (history) is passed from capture. "Parent" is the board above the child board,
-- and "state" is a board being passed into evaluateBoard.
miniMax :: Char -> Char -> Int -> Int -> Int -> Boards -> Board -> Board -> EBoard
miniMax player turn depth size dim hist parent state
    | depth == 0                = final
    | null vMoves               = if player == turn then (parent, (-100)) else (parent, 100)
    | endgame                   = if player == turn then (parent, 100) else (parent, (-100))
    | player == turn            = get (<) nextmap
    | otherwise                 = get (>) nextmap
    where nextmap = (map (miniMax player (opposite turn) (depth-1) size dim hist parent) vMoves)
          vMoves = (validMoves hist (moveGen state dim  size turn) size)
          endgame = not (null (filter (\x -> (evaluateBoard x turn dim) == 100) vMoves))
          final =  (parent, evaluateBoard state player dim)


-- Here we evaluate the board ("state") passed in based on whose turn ("letter") it is
-- and the "dim" (dimension) given of the board. We use the dimension specifically to be
-- passed into flagDistance, which needs it to calculate one of our heuristics
-- (how far the flag is from reaching the other side vs. if the flag has already reached
-- the other side, which would be a win state).
evaluateBoard :: Board -> Char -> Int -> Int
evaluateBoard state player dim
    | win             = 100
    | pieces > 0      = distance
    | otherwise       = pieces
    where capture'    = captured state (opposite player) -- win state
          zero        = zeroPcs state (opposite player)  -- win state
          pieces      = numPcs state player              -- normal state
          distance    = flagDistance state player 0 dim  -- normal state/win state
          win         = maximum [distance, capture', zero] == 100

-- A bit faster than quicksort. We use this in our miniMax algorithm to select
-- scores as they get propagated up the tree. It is tail recursive
-- and uses a helper function to separate the head "current" from the
-- tail "boards" so that we can select based on heuristic scores.
get :: (Int->Int->Bool) -> EBoards -> EBoard
get _ [] = ([],0)
get func (b:bs) = get_helper func bs b

get_helper :: (Int->Int->Bool) -> EBoards -> EBoard -> EBoard
get_helper func boards current
    | null boards = current
    | apply func current (head boards) = get_helper func (tail boards) (head boards)
    | otherwise = get_helper func (tail boards) current

-- Our maxSort is a version of quicksort that is used to sort evaluated boards.
maxSort :: EBoards -> EBoards
maxSort []      = []
maxSort (x:xs)  = maxSort (filter (apply (<=) x) xs) ++ [x] ++ maxSort (filter (apply (>) x) xs)

-- Apply is for applying functions to the heuristic scores of boards.
apply :: (Int -> Int -> Bool) -> EBoard -> EBoard -> Bool
apply func board1 board2 = func (snd board1) (snd board2)

-- Heuristic Evaluators
-- Heuristic #1: Number of Pieces on Board
-- Compare how many pieces each color has to see who may be closer to winning.
-- Uses a helper function and tail recursion to keep track of two accumulating values.
numPcs :: Board -> Char -> Int
numPcs state turn = numPcsHelper state turn 0 0

numPcsHelper :: Board -> Char -> Int -> Int -> Int
numPcsHelper state player count countOp
    | null state            = round ((fromIntegral count - fromIntegral countOp)) * 10
    | spotPc == opponent
        || spotPc == toUpper opponent
        || spotPc == toLower opponent
                            = numPcsHelper unsearched player count (countOp + 1)
    | spotPc == player
        || spotPc == toUpper player
        || spotPc == toLower player
                            = numPcsHelper unsearched player (count + 1) countOp
    | otherwise             = numPcsHelper unsearched player count countOp
    where opponent = opposite player
          spotPc = head state
          unsearched = tail state

-- Heuristic #2: Flag Distance
-- Gives points up to the size of the board for how close the turn's flag is to
-- the opposite side
-- WIN STATE: When flag reaches opposite side of board
flagDistance :: Board -> Char -> Int -> Int -> Int
flagDistance [] _ _ _ = 0
flagDistance (x:xs) turn pos dim
    | turn == 'w' && x == 'W' = round ((fromIntegral rowPos + 1) / (fromIntegral dim) * 100)
    | turn == 'b' && x == 'B' = round (fromIntegral (dim - rowPos) / (fromIntegral dim) * 100)
    | otherwise               = flagDistance xs turn (pos + 1) dim
    where rowPos = fst (rowCol pos dim)


-- WIN STATE: If the opponent no longer has any pawns
zeroPcs :: Board -> Char -> Int
zeroPcs state pawns
    | elem pawns state             = 0
    | otherwise                    = 100 -- the other side has no pawns


-- WIN STATE: If the opponent's flag was captured
captured :: Board -> Char -> Int
captured state letter
    | elem (toUpper letter) state  = 0 -- flag was captured
    | otherwise                    = 100


-- WIN STATE: When the opponent has no valid moves left based on the history given
-- Checks for valid moves by checking that the size of the boards match the
-- original size
validMoves :: Boards -> Boards -> Int -> Boards
validMoves history evaList size = filter ((\ x y z -> x == (length z) && not (elem z y)) size history) evaList


-- Moves Generator: generates the possible moves for a given turn.
-- Assume size of board has already been sqrt'd to dim
-- Uses the current state, dimension, size, and turn.
moveGen :: Board -> Int -> Int -> Char -> Boards
moveGen currState dim size letter =  moveGenHelper currState currState dim 0 letter

moveGenHelper :: Board -> Board -> Int -> Int -> Char -> Boards
moveGenHelper currState mutState dim pos player
    | null mutState          = []
    | spotPc == player ||
      spotPc == toUpper player
      = (move currState spotPc dim coor) ++ moveGenHelper currState unsearched dim (pos + 1) player
    | otherwise
      = moveGenHelper currState unsearched dim (pos + 1) player
    where spotPc = head mutState
          coor = rowCol pos dim
          unsearched = tail mutState


-- Performs all moves
-- Checks that the list of possible moves are in bounds, and if they are, it performs them
-- through performMove or performJump.
move :: Board -> Char -> Int -> Coordinate -> Boards
move currState turn dim coord = moveHelper currState turn dim coord (possPos turn dim coord)

moveHelper :: Board -> Char -> Int -> Coordinate -> [Move] -> Boards
moveHelper state turn dim coord possMoves
    | null possMoves = []
    | otherwise      = doMove : moveHelper state turn dim coord (tail possMoves)
    where doMove = performMove state (fst (head possMoves)) coord (snd (head possMoves)) 0 dim turn


-- Is the move within bounds of the board?
-- Use the position of the piece moving and the dimensions
-- of the board to determine this.
inBounds :: Coordinate -> Int -> Bool
inBounds (row, col) dim = row >= 0 && col >= 0 && row < dim && col < dim


-- Perform the move and return the altered state.
-- This is for non-jump moves, indicated by 'm'.
performMove :: Board -> Coordinate -> Coordinate -> Char -> Int -> Int -> Char -> Board
performMove currState targetPos pcPos 'm' viewPos dim piece
    | null currState  || targetNotEmpty  = []
    | coor == targetPos                 = piece : doNextMove
    | coor == pcPos                     = '-' : doNextMove
    | otherwise                         = head currState : doNextMove
    where coor           = rowCol viewPos dim
          doNextMove     = performMove (tail currState) targetPos pcPos 'm' (viewPos + 1) dim piece
          targetNotEmpty = (coor == targetPos && head currState /= '-')

performMove currState targetPos pcPos 'j' viewPos dim piece
    = performJump currState targetPos pcPos viewPos dim piece (checkJumpee pcPos targetPos)

-- Perform jump moves, indicated by 'j'. Only for pawns.
performJump :: Board -> Coordinate -> Coordinate -> Int -> Int -> Char -> Coordinate -> Board
performJump currState targetPos pcPos viewPos dim piece jumpee
    | null currState || targetNotEmpty || noJumpee  = []
    | coor == targetPos                                 = piece : doNextJump
    | coor == pcPos || coor == jumpee                   = '-' : doNextJump
    | otherwise                                         = head currState : doNextJump
    where targetNotEmpty    = (coor == targetPos && head currState /= '-')
          noJumpee          = (coor == jumpee && (toLower (head currState) /= opposite piece))
          coor              = rowCol viewPos dim
          doNextJump        = performJump (tail currState) targetPos pcPos (viewPos + 1) dim piece jumpee


-- Check that we can jump, given the piece's position
-- and the target position.
checkJumpee :: Coordinate -> Coordinate -> Coordinate
checkJumpee (pRow, pCol) (tRow, tCol)
    | pRow == tRow = (pRow , div (pCol + tCol) 2)
    | pCol == tCol = (div (pRow + tRow) 2 , pCol)
    | otherwise    = (-1, -1)


-- Returns the opposite character of the piece given, useful in
-- many cases for our functions.
opposite :: Char -> Char
opposite letter
    | letter == 'b' = 'w'
    | letter == 'w' = 'b'
    | otherwise     = '-'


-- Coordinates Developer
-- Returns the row and column of an indicated position
-- Assume the dimension has already been sqrt'd to dim
rowCol :: Int -> Int -> Coordinate
rowCol pos dim = ((pos `div` dim), (pos `mod` dim))


-- Positions Generator
-- Captures all possible moves for flags and pawns based on the rules of the board CTF (white at top and black at bottom)
-- Later gets validated based on dimension of board in another function above.
possPos :: Char -> Int -> Coordinate -> [Move]
possPos piece dim (row, col)
    | piece == 'w'                  = filter bounded [jumpdown, jumpleft,   jumpright,  down,   right,  left]
    | piece == 'b'                  = filter bounded [jumpup,   jumpright,  jumpleft,   up,     left,   right]
    | piece == 'W'                  = filter bounded [down, right,  left,   up]
    | piece == 'B'                  = filter bounded [up,   left ,  right,  down]
    | otherwise    = []
    where up    = ((row - 1, col), 'm')
          down  = ((row + 1, col), 'm')
          left  = ((row, col - 1), 'm')
          right = ((row, col + 1), 'm')
          jumpup    = ((row - 2, col), 'j')
          jumpdown  = ((row + 2, col), 'j')
          jumpleft  = ((row, col - 2), 'j')
          jumpright = ((row, col + 2), 'j')
          bounded = \x -> inBounds (fst x) dim
