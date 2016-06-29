module SolitaireAI where
import System.Random
import Solitaire
import Data.Maybe
import Data.List

--Takes an EOBoard and returns a list of EOBoards containing all boards that are one move away from the board passed. 
--Head-cards of every Column are selected, unless a Column has only one card and it is a King,
-- and an attempt is made to move every selected card to the column whose head card is the successor 
--of the card selected. The cards can also move to the reserves.  
findMoves :: EOBoard -> [EOBoard]
findMoves board@(foundations,columns,reserve)
    |cardsCanMove = filter (/= board) ((map (moveCardColumns board) selectedCards) ++ (map (moveCardReserves board) (selectHeadsC columns))) --doesnt move to reserves yet
    |otherwise = []
    where selectedCards = selectHeadsC columns  --heads of columns
          cardsCanMove = any (cardCanMove columns) selectedCards || emptyReserveExists --returns true if any of the selectedCards can move
          cardCanMove columns card = isKing card && emptyColumnExists || not(isKing card) && nonKingCardCanMove columns card
          emptyColumnExists = length columns < 8 
          emptyReserveExists = length reserve < 8

--Takes columns and a card (non-King) and returns true if the card can move to a column.
--The card can move to a column if its successor is the head of that column
nonKingCardCanMove :: Columns -> Card -> Bool
nonKingCardCanMove columns card = elem (sCard card) (map head (filter (not.null) columns))

--Takes columns and returns the list of all head-cards unless the head card of a column of length 1 is a King
--This is because a King which is the top-card of a column is in the right position and
--shouldn't be moved.
selectHeadsC :: [[Card]] -> [Card]
selectHeadsC columns = [head column| column <- columns, (not . null) column, not (topCardisKing column)]
    where topCardisKing column = isKing (head column) && (length column==1)

--Takes and EOBoard and calls findMoves. Of the list of boards returned by findMoves, chooseMoves
--picks the board resulting after the "best move" as determined by the weighting function addWeight.
chooseMove :: EOBoard -> Maybe EOBoard
chooseMove board@(foundations,columns,reserve)
    |(not.null) completed_boards = Just bestMove
    |otherwise = Nothing
    where completed_boards = map toFoundations (findMoves board)
          weighted_boards = map addWeight completed_boards
          bestMove = head (map fst $ sortBy (\(_,n1) (_,n2) -> compare n1 n2) (zip completed_boards weighted_boards))

--Assigns a weight to a board based on the number of cards that are in foundations.
--Higher the number of cards of the foundations, higher the weight.
addWeight :: EOBoard -> Int
addWeight board@(foundations,columns,reserve) = sum (pipValuefoundations) + length (topKings (filter (not.null) columns))
     where pipValuefoundations = map turnToIntegers (map head (filter (not.null) foundations))

--returns a list containing Kings which are top cards in their columns
topKings :: [[Card]] -> [Card]
topKings [] = []
topKings (firstColumn:restColumns) 
    |isKing card = (card:topKings restColumns)
    |otherwise = topKings restColumns
    where card = last firstColumn

--Takes a card and returns an integer value propertional to the Pip value of the card 
turnToIntegers :: Card -> Int
turnToIntegers (pip, _) = turnToIntegersA pip Ace 1

turnToIntegersA :: Pip -> Pip -> Int -> Int
turnToIntegersA King pip current = 13
turnToIntegersA pip current count
    | pip == current = count
    | otherwise = turnToIntegersA pip (succ current) (count+1)

--Takes a board and a card and returns the board resulting from moving the card to a column, if it can be moved.
moveCardColumns :: EOBoard -> Card -> EOBoard
moveCardColumns board@(foundations,columns,reserve) card 
    |isKing card && length columns < 8 = (foundations,[card]:(remove card columns), delete card reserve)
    |not (isKing card) && (nonKingCardCanMove columns card) = moveNonKing (foundations,remove card columns,delete card reserve) card
    |otherwise = board  

--Takes a card (non-King) and an EOBoard and moves the given card to the right column
-- if it can be moved and returns the resulting EOBoard
moveNonKing :: EOBoard-> Card -> EOBoard
moveNonKing (foundations,[],reserve) card = (foundations,[],reserve)
moveNonKing (foundations,columns,reserve) card
--check if successor of given card is the head of the first column, if it is, move the card to the column
-- If successor of card is not head of the first deck in column, recurse with remaining columns. 
--Return original EOBoard if given card cannot be moved.
--Since sCard will never be used for an King, the error condition is always avoided.
    | head firstColumn == sCard card  = (foundations,((card:firstColumn):tailColumns),reserve) 
    | otherwise = (updatedFoundations,(firstColumn:updatedColumns), updatedReserve)
    where (firstColumn:tailColumns) = filter (not.null) columns
          (updatedFoundations, updatedColumns, updatedReserve) = moveNonKing (foundations,tailColumns,reserve) card

    
--moves the given card to reseves if it can be moved. 
--Cards are not moved if reserves already have atleast 5 cards
moveCardReserves :: EOBoard -> Card -> EOBoard
moveCardReserves (foundations,columns,[]) card  = (foundations,remove card columns,[card])
moveCardReserves board@(foundations,columns,reserve) card
    | length reserve < 5 = (updatedFoundations,updatedColumns,firstCard:updatedReserve)
    | otherwise = board
    where (firstCard:restCards) = reserve
          (updatedFoundations, updatedColumns, updatedReserve) = moveCardReserves (foundations,columns,restCards) card 

--Takes an EOBoard and returns an Int equal to the score after playing a game to completion            
eOGame :: EOBoard -> Int
eOGame board
    | isNothing nextBoard = calcScore board
    | otherwise = eOGame (resMaybe nextBoard)
    where nextBoard = chooseMove board
          calcScore (foundations,columns,reserve) = sum $ map length foundations

--Takes a seed and the number of games to be played 
--Returns the number of wins and the average score as a tuple
eOExpt :: Int -> Int -> (Int,Float)
eOExpt seed numberOfGames = (0,avScore)
     where listBoards = map eODeal (take 100 (randoms (mkStdGen seed) :: [Int]))
           listScores = map eOGame listBoards
           avScore = fromIntegral (sum listScores)/ fromIntegral numberOfGames

 ----------------------------------------------------------
 -- display an EOBoard
displayEOB :: EOBoard -> IO String
 
displayEOB (fnds,cols,res) = do
  let colStr = colsToString cols
  putStr "EOBoard\nFoundations  "
  putStrLn (show fnds)
  putStr  "Columns"
  putStr colStr
  putStr "\n\nReserve     "
  putStrLn (show (((sum (map (fromIntegral.length) cols))+(sum (map (fromIntegral.length) fnds))+(fromIntegral (length res)))))
  putStrLn (show res)
  putStr "\n---------------------------------------------\n"
  return ""

colsToString :: Columns->String -- prepare String to print columns on separate lines
 
colsToString cols =
  foldr (++) "" ["\n             "++(show col) |col<-cols]
  
-----------------------------------------------------------------------

-- display a list of EOBoards  

displayEOBList :: [EOBoard]-> IO String
 
displayEOBList eobl =  -- @ notation doesn't seem to work correctly
  do
   if (null eobl) then do (return "")
                  else do
                        displayEOB (head eobl)
                        displayEOBList (tail eobl)
 
   
-----------------------------------------------------------------

 --scoreBoard
 -- score is number of cards on foundations
 -- return a String for display
 
scoreBoard :: EOBoard-> String 
scoreBoard (fnds, cols, res) = "A LOSS: SCORE  " ++ (show (52- (length res) - (foldr (+) 0 (map length cols))))      

 -----------------------------------------------------------------------------
 -- play a game given initial board
 -- assuming a fn chooseMove :: EOBoard ->Maybe EOBoard
 -- & that toFoundations is handled outside
 
displayEOGame :: EOBoard ->IO String
 
displayEOGame b = do
  let (fnds,cols,res) = b -- apparently can't do this with @
  if ((null cols)&&(null res)) -- if cols & reserve empty its a win
     then return "A WIN"
     else 
      do
       displayEOB b -- display given board
       let res = chooseMove b
       if (isJust res) then
               do
                let nb = resMaybe res
                displayEOGame nb
              else
               do
                 let score = scoreBoard b
                 return score
 
---- ------------------------------------------------  
--Maybe helper                
resMaybe :: (Maybe a) -> a
resMaybe (Just x) = x 
 
--    