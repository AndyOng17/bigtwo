{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Game where

import PokerCards 
import RandomCard
import System.Random (newStdGen)
import Relude (maybeToRight)
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.List (delete)

newtype B2Card = B2 { getCard :: Card } deriving Eq

instance Show B2Card where
    show (B2 card) = show card

type B2Deck = [B2Card]

instance Ord B2Card where
    compare (B2 (Card Two twoSuit)) (B2 (Card cardNum cardSuit))
        | cardNum == Two = compare twoSuit cardSuit
        | otherwise = GT
    compare (B2 (Card cardNum cardSuit)) (B2 (Card Two twoSuit))
        | cardNum == Two = compare cardSuit twoSuit
        | otherwise = LT
    compare (B2 card1) (B2 card2) = compare card1 card2


data FiveCard = FiveCard B2Card B2Card B2Card B2Card B2Card deriving (Eq, Show)

data Play = Single B2Card
          | Pair B2Card B2Card
          | Triple B2Card B2Card B2Card
          | Combo B2Card B2Card B2Card B2Card B2Card
          | Pass
          deriving Eq


instance Show Play where
    show (Single (B2 card)) = "Single " ++ show card
    show (Pair (B2 card1) (B2 card2)) = "Pair " ++ show card1 ++ " " ++ show card2
    show (Triple (B2 card1) (B2 card2) (B2 card3)) = "Triple " ++ show card1 ++ " " ++ show card2 ++ " " ++ show card3
    show (Combo (B2 card1) (B2 card2) (B2 card3) (B2 card4) (B2 card5)) =
        "Combo " ++ show card1 ++ " " ++ show card2 ++ " " ++ show card3 ++ " " ++ show card4 ++ " " ++ show card5
    show Pass = show Pass

comparePlays :: Play -> Play -> Maybe Ordering
comparePlays
    (Single fstCard)
    (Single sndCard) = Just $ compare fstCard sndCard
comparePlays
    (Pair fstCard1@(B2 (Card fstNum1 fstSuit1)) (B2 (Card fstNum2 fstSuit2)))
    (Pair sndCard1@(B2 (Card sndNum1 _)) (B2 (Card sndNum2 _)))
    | notPairs = Nothing
    | sameNumOnBothPairs = if Spade `elem` [fstSuit1, fstSuit2] then Just GT else Just LT
    | otherwise = Just $ compare fstCard1 sndCard1
    where notPairs = not $ fromEnum fstNum1 == fromEnum fstNum2 && fromEnum sndNum1 == fromEnum sndNum2
          sameNumOnBothPairs = all (== fromEnum fstNum1) [fromEnum fstNum2, fromEnum sndNum1, fromEnum sndNum2]
comparePlays
    (Triple fstCard1@(B2 (Card fstNum1 _)) (B2 (Card fstNum2 _)) (B2 (Card fstNum3 _)))
    (Triple sndCard1@(B2 (Card sndNum1 _)) (B2 (Card sndNum2 _)) (B2 (Card sndNum3 _)))
    | notTriples = Nothing
    | otherwise = Just $ compare fstCard1 sndCard1
    where notTriples = not $ all ((== fromEnum fstNum1) . fromEnum) [fstNum2, fstNum3]
                          && all ((== fromEnum sndNum1) . fromEnum) [sndNum2, sndNum3]
comparePlays
    (Combo (B2 fstComboCard1) (B2 fstComboCard2) (B2 fstComboCard3) (B2 fstComboCard4) (B2 fstComboCard5))
    (Combo (B2 sndComboCard1) (B2 sndComboCard2) (B2 sndComboCard3) (B2 sndComboCard4) (B2 sndComboCard5)) = do 
        combo1Type <- mbCombo1Type
        combo2Type <- mbCombo2Type
        let sameComboType = combo1Type == combo2Type
        return $ if not sameComboType 
                 then compare combo1Type combo2Type
                 else GT
    where mbCombo1Type = handType fstComboCard1 fstComboCard2 fstComboCard3 fstComboCard4 fstComboCard5
          mbCombo2Type = handType sndComboCard1 sndComboCard2 sndComboCard3 sndComboCard4 sndComboCard5
comparePlays _ _ = Nothing

getCardsFromPlay :: Play -> [B2Card]
getCardsFromPlay Pass = []
getCardsFromPlay (Single card1) = [card1]
getCardsFromPlay (Pair card1 card2) = [card1, card2]
getCardsFromPlay (Triple card1 card2 card3) = [card1, card2, card3]
getCardsFromPlay (Combo card1 card2 card3 card4 card5) = [card1, card2, card3, card4, card5]

playToPlayType :: Play -> String
playToPlayType (Single {}) = "Single"
playToPlayType (Pair {}) = "Pair"
playToPlayType (Triple {}) = "Triple"
playToPlayType (Combo {}) = "Combo"
playToPlayType Pass = "Pass"


data Discard = Discard { trick :: Int
                       , play :: Play
                       , playedBy :: Player }
               deriving Eq

instance Show Discard where
    show (Discard aTrick aPlay aPlayer)
        =  "Trick: [" ++ show aTrick
        ++ "] Player: [" ++ show aPlayer
        ++ "] Cards Played: [" ++ show aPlay
        ++ "]\n"

newtype Discards = Discards { getDiscards :: [Discard] } deriving Eq

instance Show Discards where
    show (Discards []) = ""
    show (Discards (discard:discards)) = show discard ++ show (Discards discards)

newtype Hand = Hand { getHand :: B2Deck } deriving (Show, Eq)

data Hands = FourP Hand Hand Hand Hand
           | ThreeP Hand Hand Hand
           | TwoP Hand Hand
           deriving (Eq)

instance Show Hands where
    show (FourP (Hand hand1) (Hand hand2) (Hand hand3) (Hand hand4))
        =  "Player 1: " ++ show hand1 ++ "\n"
        ++ "Player 2: " ++ show hand2 ++ "\n"
        ++ "Player 3: " ++ show hand3 ++ "\n"
        ++ "Player 3: " ++ show hand4 ++ "\n"
    show (ThreeP (Hand hand1) (Hand hand2) (Hand hand3))
        =  "Player 1: " ++ show hand1 ++ "\n"
        ++ "Player 2: " ++ show hand2 ++ "\n"
        ++ "Player 3: " ++ show hand3 ++ "\n"
    show (TwoP (Hand hand1) (Hand hand2))
        =  "Player 1: " ++ show hand1 ++ "\n"
        ++ "Player 2: " ++ show hand2 ++ "\n"

data Seat = North | East | South | West deriving (Show, Eq, Enum)

data Player = P1 | C1 | P2 | C2 | P3 | C3 | P4 | C4 deriving (Show, Eq)

data Setting = Setting (Maybe Player) (Maybe Player) (Maybe Player) (Maybe Player)

playerToHand :: Player -> Hands -> Either ErrorMsg Hand
playerToHand player hands = maybeToRight "Player and Hands provided mismatch" $ seatToHand (playerToSeat player) hands

seatToHand :: Seat -> Hands -> Maybe Hand
seatToHand seat (FourP north east south west) = case seat of
    North -> Just north
    East -> Just east
    South -> Just south
    West -> Just west
seatToHand seat (ThreeP north east south) = case seat of
    North -> Just north
    East -> Just east
    South -> Just south
    _ -> Nothing
seatToHand seat (TwoP north east) = case seat of
    North -> Just north
    East -> Just east
    _ -> Nothing

playerToSeat :: Player -> Seat
playerToSeat P1 = North
playerToSeat C1 = North
playerToSeat P2 = East
playerToSeat C2 = East
playerToSeat P3 = South
playerToSeat C3 = South
playerToSeat P4 = West
playerToSeat C4 = West

type Winner = Player

data Status = Alive Discards Hands
            | Ended Winner Discards Hands
            deriving Eq

instance Show Status where
    show (Alive discards hands)
        =  "Game In Progress \n"
        ++ "Discard Pile: \n"
        ++ show discards ++ "\n"
        ++ show hands ++ "\n"
    show (Ended winner discards hands)
        =  "Game Ended \n"
        ++ "Discard Pile: \n"
        ++ show discards ++ "\n"
        ++ show hands ++ "\n"
        ++ "Winner: " ++ show winner

type ErrorMsg = String

nextPlayer :: Int -> Player -> Seat
nextPlayer 2 P2 = North
nextPlayer 2 C2 = North
nextPlayer 3 P3 = North
nextPlayer 3 C3 = North
nextPlayer 4 P4 = North
nextPlayer 4 C4 = North
nextPlayer _ currP = succ $ playerToSeat currP

isRightPlayer :: Seat -> Player -> Bool
isRightPlayer North player = player `elem` [P1, C1]
isRightPlayer East player = player `elem` [P2, C2]
isRightPlayer South player = player `elem` [P3, C3]
isRightPlayer West player = player `elem` [P4, P4]

whoseTurn :: Status -> Maybe Seat
whoseTurn (Alive (Discards []) _) = Just North
whoseTurn (Alive discards hands) = Just $ nextPlayer (getNoOfPlayers hands) (latestPlayer discards)
whoseTurn (Ended {}) = Nothing

latestPlayer :: Discards -> Player
latestPlayer = playedBy . head . getDiscards

getNoOfPlayers :: Hands -> Int
getNoOfPlayers (FourP {}) = 4
getNoOfPlayers (ThreeP {}) = 3
getNoOfPlayers (TwoP {}) = 2

gameEndedMsg :: ErrorMsg
gameEndedMsg = "Plays can't be made when game has ended"

processPlay :: Player -> Status -> Play -> Either ErrorMsg Status
processPlay _ (Ended {}) _ = Left gameEndedMsg
processPlay aPlayer status@(Alive discards hands) aPlay
    | not isPlayerTurn = Left $ "It's not " ++ show aPlayer ++ "'s turn"
    | isPassOnNewTrick = Left "Can't pass when leading a trick"
    | isWrongPlayType = Left "Wrong play type"
    | otherwise = updateStatus aPlayer aPlay status
    where isPlayerTurn = maybe True (`isRightPlayer` aPlayer) (whoseTurn status)
          isPassOnNewTrick = isStartingNewTrick && aPlay == Pass
          isWrongPlayType = fromMaybe False $ do
            currPlayTyp <- playTypeOfCurrTrick status -- completely new game when Nothing
            return $ not isStartingNewTrick && currPlayTyp /= playToPlayType aPlay
          isStartingNewTrick = isNewTrick status
          lastDiscard = fmap play . listToMaybe . getDiscards $ discards
          isLegalFollowUp = if isStartingNewTrick then Just True else undefined

updateStatus :: Player -> Play -> Status -> Either ErrorMsg Status
updateStatus _ _ Ended {} = Left gameEndedMsg
updateStatus aPlayer aPlay status@(Alive discards hands) = do
    handToUpdate <- playerToHand aPlayer hands
    updatedHand <- removePlay aPlay handToUpdate
    let updatedDiscards =  Discards $ Discard trickNo aPlay aPlayer : getDiscards discards
        isUpdatedHandEmpty = null $ getHand updatedHand
    if isUpdatedHandEmpty
    then updateHands (playerToSeat aPlayer) updatedHand hands >>= (return . Ended aPlayer updatedDiscards)
    else updateHands (playerToSeat aPlayer) updatedHand hands >>= (return . Alive updatedDiscards)
    where isNewGame = null $ getDiscards discards
          isStartingNewTrick = isNewTrick status
          trickNo | isNewGame = 1
                  | isStartingNewTrick = (+1) . trick . head . getDiscards $ discards
                  | otherwise = trick . head . getDiscards $ discards

updateHands :: Seat -> Hand -> Hands -> Either ErrorMsg Hands
updateHands seat hand (FourP north east south west) = case seat of
    North -> Right $ FourP hand east south west
    East -> Right $ FourP north hand south west
    South -> Right $ FourP north east hand west
    West -> Right $ FourP north east south hand
updateHands seat hand (ThreeP north east south) = case seat of
    North -> Right $ ThreeP hand east south
    East -> Right $ ThreeP north hand south
    South -> Right $ ThreeP north east hand
    _ -> Left $ "Seat " ++ show seat ++ " is not in play"
updateHands seat hand (TwoP north east) = case seat of
    North -> Right $ TwoP hand east
    East -> Right $ TwoP north hand
    _ -> Left $ "Seat " ++ show seat ++ " is not in play"

removePlay :: Play -> Hand -> Either ErrorMsg Hand
removePlay Pass _ = Left "'Pass' can't be removed from a hand"
removePlay aPlay aHand = foldr removeCard' (Right aHand) (getCardsFromPlay aPlay)
    where removeCard' cardToRemove hand = hand >>= removeCard cardToRemove

removeCard :: B2Card -> Hand -> Either ErrorMsg Hand
removeCard _ (Hand []) = Left "Empty hand"
removeCard card (Hand hand) = if card `elem` hand
                              then Right . Hand $ delete card hand
                              else Left $ "Attempting to remove "
                                        ++ cardToString (getCard card)
                                        ++ " that can't be found in the hand"

isPlayInHand :: Play -> Hand -> Maybe Bool
isPlayInHand Pass _ = Nothing
isPlayInHand (Single card1) (Hand deck) = Just $ card1 `elem` deck
isPlayInHand (Pair card1 card2) (Hand deck) = Just $ card1 `elem` deck && card2 `elem` deck
isPlayInHand (Triple card1 card2 card3) (Hand deck) = Just $ card1 `elem` deck && card2 `elem` deck && card3 `elem` deck
isPlayInHand (Combo card1 card2 card3 card4 card5) (Hand deck) =
    Just $ card1 `elem` deck && card2 `elem` deck && card3 `elem` deck && card4 `elem` deck && card5 `elem` deck

isNewTrick :: Status -> Bool
isNewTrick (Ended {}) = False
isNewTrick (Alive (Discards []) _) = True
isNewTrick (Alive (Discards discards) FourP {})
    | length discards <= 4 = False
    | otherwise = let (lastPlay1:lastPlay2:lastPlay3:_) = discards
                  in play lastPlay1 == Pass || play lastPlay2 == Pass || play lastPlay3 == Pass
isNewTrick (Alive (Discards discards) ThreeP {})
    | length discards <= 3 = False
    | otherwise = let (lastPlay1:lastPlay2:_) = discards
                  in play lastPlay1 == Pass || play lastPlay2 == Pass
isNewTrick (Alive (Discards discards) TwoP {})
    | length discards <= 2 = False
    | otherwise = let (lastPlay1:_) = discards
                  in play lastPlay1 == Pass

playTypeOfCurrTrick :: Status -> Maybe String
playTypeOfCurrTrick (Alive (Discards []) _) = Nothing
playTypeOfCurrTrick (Ended {}) = Nothing
playTypeOfCurrTrick (Alive (Discards discards) _) =
    Just . playToPlayType . play . head . dropWhile ((== Pass) . play) $ discards

newGame :: Int -> B2Deck -> Status
newGame n deck
    | n > 4 || n < 2 = error "Invalid number of players"
    | otherwise = Alive (Discards []) $ dealCard n deck

runGame :: IO ()
runGame = do
    gen <- newStdGen
    let newShuffledDeck = B2 <$> genFullDeckShuffled gen
        newGameStatus = newGame 4 newShuffledDeck
    return ()

dealCard :: Int -> B2Deck -> Hands
dealCard _ deck | length deck /= 52 = error "Deck with incorrect amount of cards had been used"
dealCard 4 deck = dealCards' 4 deck (FourP (Hand []) (Hand []) (Hand []) (Hand []))
dealCard 3 deck = dealCards' 3 deck (ThreeP (Hand []) (Hand []) (Hand []))
dealCard 2 deck = dealCards' 2 deck (TwoP (Hand []) (Hand []))
dealCard _ _ = error "Invalid number of players specified"

dealCards' :: Int -> B2Deck -> Hands -> Hands
dealCards' 4 deck hands@(FourP (Hand hand1) (Hand hand2) (Hand hand3) (Hand hand4)) = case splitAt 4 deck of
    ([card1, card2, card3, card4], restOfDeck) -> dealCards' 4 restOfDeck $ FourP (Hand $ card1:hand1) (Hand $ card2:hand2) (Hand $ card3:hand3) (Hand $ card4:hand4)
    ([], _) -> hands
    _ -> error "Deck with incorrect amount of cards had been used"
dealCards' 3 deck (ThreeP (Hand hand1) h2@(Hand hand2) h3@(Hand hand3)) = case splitAt 3 deck of
    ([card1, card2, card3], restOfDeck) -> dealCards' 3 restOfDeck $ ThreeP (Hand $ card1:hand1) (Hand $ card2:hand2) (Hand $ card3:hand3)
    ([lastCard], _) -> ThreeP (Hand $ lastCard:hand1) h2 h3
    _ -> error "Deck with incorrect amount of cards had been used"
dealCards' 2 deck hands@(TwoP (Hand hand1) (Hand hand2)) = case splitAt 2 deck of
    ([card1, card2], restOfDeck) -> dealCards' 2 restOfDeck $ TwoP (Hand $ card1:hand1) (Hand $ card2:hand2)
    ([], _) -> hands
    _ -> error "Deck with incorrect amount of cards had been used"
dealCards' _ _ _ = error "Invalid number of players specified"

sampleGame :: IO Status
sampleGame = shuffledDeck >>= (return . newGame 4)

shuffledDeck :: IO B2Deck
shuffledDeck = newStdGen >>= return . fmap B2 . genFullDeckShuffled

sampleStatus :: Status
sampleStatus = newGame 4 (fmap B2 fullDeck)
