module Solitaire where
import System.Random
import Data.List

--algebraic data type for Suits in a pack of cards
data Suit = Hearts | Clubs | Spades | Diamonds
            deriving (Eq, Show)
--algebraic data type for pip values in a pack of cards
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
           deriving (Eq, Ord, Enum, Show)

--A card has a tuple of Pip and Suit
type Card = (Pip, Suit)

--A Deck is a list of Cards
type Deck = [Card]

--Reserve is a list of Cards
type Reserve = [Card]

--Columns are a list of lists of Cards
type Columns = [[Card]]

--Foundations are a list of lists of Cards
type Foundations = [[Card]]

--EOBoard is a triple of Foundations, Columns and Reserve
type EOBoard = (Foundations, Columns, Reserve)

--A pack is a list of all 52 Cards
pack :: [Card]
pack = [(pip,suit)| suit <- [Hearts,Clubs,Diamonds,Spades], pip <- [Ace ..]]

--returns the successor card of the card given
sCard :: Card -> Card
sCard (pip, suit) = (succ pip,suit) 

--returns the predecessor card of the card given
pCard :: Card -> Card
pCard (pip, suit) = (pred pip,suit)

--returns true if given card is an Ace, and false otherwise
isAce :: Card -> Bool
isAce (pip,_) = pip == Ace

--returns true if given card is a King, and false otherwise
isKing :: Card -> Bool
isKing (pip,_) = pip == King 

--returns a shuffled pack by zipping random numbers with every card in the pack and ordering the
--resulting tuples by those numbers. By extracting the first element of every tuple, we get a list of 52 shuffled
--cards.
shuffle :: Int -> Deck
shuffle seed  = map fst $ sortBy (\(_,n1) (_,n2) -> compare n1 n2) (zip pack random_list)
               where random_list = take 52 (randoms (mkStdGen seed) :: [Int])

--returns a shuffled EOBoard
eODeal :: Int -> EOBoard
eODeal seed  = ([],shuffled_columns,shuffled_reserve)
          where shuffled_reserve = take 4 (shuffle seed)--first 4 cards in a shuffled pack taken as Reserves
                shuffled_columns = splitPack $ drop 4 (shuffle seed) --groups remmaining 48 cards into lists of 6 cards each

--splits a list into groups of six, used for creating Columns 
splitPack :: [a] -> [[a]]
splitPack [] = []
splitPack deck = (h: splitPack t)
                 where (h,t) = splitAt 6 deck

--toFoundations takes an EOBoard and returns an EOBoard after making all possible moves to the Foundations from the
--Columns and Reserve by selecting head-cards of every Column and all cards in Reserve and attempting to
--move them to the Foundations. The function repeats this procedure until no head-card or card from the reserves
--can be moved. When no cards can be moved, the resulting EOBoard is returned. 
toFoundations :: EOBoard -> EOBoard
toFoundations board@(foundations,columns,reserve)
--cardsCanMove returns true if any card from the column-heads or reserves can be moved.
--if cardsCanMove returns true, recurse by passing in resulting EOBoard to toFoundations after the cards selected in the
--previous iteration have been moved. Otherwise, return the EOBoard as was passed in.
    | cardsCanMove = toFoundations updatedEOBoard 
    | otherwise = board
    where selectedCards = selectHeads columns ++ reserve --heads of Columns plus reserves
          cardsCanMove = any (cardCanMove foundations) selectedCards --returns true if any of the selectedCards can move

          --a card can move if it is an Ace or if its predecessor card is the head of any Foundation 
          cardCanMove foundations card = isAce card || elem (pCard card) (map head foundations)

          --returns resulting board after movable cards have been moved to the Foundations
          updatedEOBoard = foldr (moveCard) board selectedCards

-- takes a list of lists and returns a list of the head of each non-empty list
selectHeads :: [[a]] -> [a]
selectHeads lists = [head list| list <- lists, (not . null) list]

--Takes a Card and an EOBoard and moves the given card to the Foundations if it can be moved and returns
--the resulting EOBoard            
moveCard :: Card -> EOBoard -> EOBoard 
moveCard card board@(foundations,columns,reserve)
--adds a new column to foundations with the card if it is an Ace, deletes the moved card from Columns/Reserves    
    |isAce card = ([card]:foundations, remove card columns, delete card reserve)
    |otherwise = moveNonAces card board 

--Takes a card (non-Ace) and an EOBoard and moves the given card to the Foundations if it can be moved and returns
--the resulting EOBoard                  
moveNonAces :: Card -> EOBoard-> EOBoard
moveNonAces card ([],columns,reserve) = ([],columns,reserve)
moveNonAces card (foundations,columns,reserve)
--check if predecessor of given card is the head of the first Foundation, if it is, add the card to the Foundation
--and remove card from Columns/Reserves. If predecessor of card is not head of the first deck in Foundations,
-- recurse with remaining Foundations. Return original EOBoard if given card cannot be moved.
--Since pCard will never be used for an Ace, the error condition is always avoided.
    | head firstFoundation == pCard card  = (((card:firstFoundation): tailFoundations), remove card columns, delete card reserve) 
    | otherwise = ((firstFoundation : updatedFoundations), updatedColumns, updatedReserve)
    where (firstFoundation:tailFoundations) = foundations
          (updatedFoundations, updatedColumns, updatedReserve) = moveNonAces card (tailFoundations,columns,reserve)

--removes a the first occurance of an item from a list of list of items. Eg: Cards from [[Card]]
remove :: Eq a => a -> [[a]] -> [[a]]
remove a [] = []
remove a (firstList:tailLists) 
    | elem a firstList = (delete a firstList : tailLists)
    | otherwise = (firstList : remove a tailLists)
  
 
meanStats :: [Int]->(Float,Float,Float)
 
meanStats lis = (mean,var,sqrt var) 
   where rlis = map fromIntegral lis
         rlen = fromIntegral (length lis)
         mean = (foldr (+) 0 rlis)/rlen
         var = (foldr (+) 0 (map (\ n -> (n-mean)**2) rlis))/rlen


                                             
                                             
                                             
 