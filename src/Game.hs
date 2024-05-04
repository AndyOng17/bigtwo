{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Game where

import qualified PokerCards as P
import Relude (maybeToRight)
import Data.List (delete, maximumBy, group)

type ErrorMsg = String

newtype B2Card = B2 { getCard :: P.Card } deriving Eq

type B2Deck = [B2Card]

instance Show B2Card where
    show (B2 card) = show card

instance Ord B2Card where
    compare (B2 (P.Card P.Two twoSuit)) (B2 (P.Card cardNum cardSuit))
        | cardNum == P.Two = compare twoSuit cardSuit
        | otherwise = GT
    compare (B2 (P.Card cardNum cardSuit)) (B2 (P.Card P.Two twoSuit))
        | cardNum == P.Two = compare cardSuit twoSuit
        | otherwise = LT
    compare (B2 card1) (B2 card2) = compare card1 card2

data FiveCard = FiveCard B2Card B2Card B2Card B2Card B2Card deriving (Eq, Show)

instance Ord FiveCard where
    compare fstHand sndHand
        | isSameHandType = compare (handRank fstHand) (handRank sndHand)
        | otherwise = compare fstHandType sndHandType
        where fstHandType = b2HandType fstHand
              sndHandType = b2HandType sndHand
              isSameHandType = fstHandType == sndHandType

handRank :: FiveCard -> Maybe B2Card
handRank fiveCard@(FiveCard b2Card1@(B2 c1) b2Card2@(B2 c2) b2Card3@(B2 c3) b2Card4@(B2 c4) b2Card5@(B2 c5))
    | isAceToFiveFlush fiveCard = Just . maximum $ cardList
    | isTwoToSixFlush fiveCard = Just . maximum $ cardList
    | P.isRoyalFlush c1 c2 c3 c4 c5 = Just . maximum $ cardList
    | P.isStraightFlush c1 c2 c3 c4 c5 = Just . maximum $ cardList
    | P.isFourOfAKind c1 c2 c3 c4 c5 = Just . maximum . maximumBy lengthOfGroup $ group cardList
    | P.isFullHouse c1 c2 c3 c4 c5 = Just . maximum . maximumBy lengthOfGroup $ group cardList
    | P.isFlush c1 c2 c3 c4 c5 = Just . maximum $ cardList
    | isAceToFive fiveCard = Just . maximum $ cardList
    | isTwoToSix fiveCard = Just . maximum $ cardList
    | P.isStraight c1 c2 c3 c4 c5 = Just . maximum $ cardList
    | otherwise = Nothing
    where lengthOfGroup group1 group2 = compare (length group1) (length group2)
          cardList = [b2Card1, b2Card2, b2Card3, b2Card4, b2Card5]

data B2HandType = AceToFiveFlush
                | TwoToSixFlush
                | RoyalFlush 
                | StraightFlush 
                | FourOfAKind
                | FullHouse
                | Flush
                | AceToFive
                | TwoToSix
                | Straight
                deriving (Eq, Show, Ord)

b2HandType :: FiveCard -> Either ErrorMsg B2HandType
b2HandType fiveCard@(FiveCard (B2 c1) (B2 c2) (B2 c3) (B2 c4) (B2 c5)) 
    | isAceToFiveFlush fiveCard = Right AceToFiveFlush
    | isTwoToSixFlush fiveCard = Right TwoToSixFlush
    | isAceToFive fiveCard = Right AceToFive
    | isTwoToSix fiveCard = Right TwoToSix
    | otherwise = do
        pokerType <- maybeToRight "Not a valid 5-card play" $ P.handType c1 c2 c3 c4 c5
        case pokerType of 
            P.RoyalFlush    -> return RoyalFlush 
            P.StraightFlush -> return StraightFlush 
            P.FourOfAKind   -> return FourOfAKind
            P.FullHouse     -> return FullHouse
            P.Flush         -> return Flush
            P.Straight      -> return Straight

isAceToFiveFlush :: FiveCard -> Bool
isAceToFiveFlush 
    (FiveCard
    (B2 (P.Card P.Ace suit1)) 
    (B2 (P.Card P.Two suit2)) 
    (B2 (P.Card P.Three suit3)) 
    (B2 (P.Card P.Four suit4)) 
    (B2 (P.Card P.Five suit5))) = all (== suit1) [suit2, suit3, suit4, suit5]
isAceToFiveFlush _ = False

isAceToFive :: FiveCard -> Bool
isAceToFive 
    (FiveCard
    (B2 (P.Card P.Ace suit1)) 
    (B2 (P.Card P.Two suit2)) 
    (B2 (P.Card P.Three suit3)) 
    (B2 (P.Card P.Four suit4)) 
    (B2 (P.Card P.Five suit5))) = not $ all (== suit1) [suit2, suit3, suit4, suit5]
isAceToFive _ = False

isTwoToSixFlush :: FiveCard -> Bool
isTwoToSixFlush 
    (FiveCard
    (B2 (P.Card P.Two suit1)) 
    (B2 (P.Card P.Three suit2)) 
    (B2 (P.Card P.Four suit3)) 
    (B2 (P.Card P.Five suit4)) 
    (B2 (P.Card P.Six suit5))) = all (== suit1) [suit2, suit3, suit4, suit5] 
isTwoToSixFlush _ = False

isTwoToSix :: FiveCard -> Bool
isTwoToSix 
    (FiveCard
    (B2 (P.Card P.Two suit1)) 
    (B2 (P.Card P.Three suit2)) 
    (B2 (P.Card P.Four suit3)) 
    (B2 (P.Card P.Five suit4)) 
    (B2 (P.Card P.Six suit5))) = not $ all (== suit1) [suit2, suit3, suit4, suit5] 
isTwoToSix _ = False

data Play = Single B2Card
          | Pair B2Card B2Card
          | Triple B2Card B2Card B2Card
          | Quintuple FiveCard
          | Pass
          deriving Eq

instance Show Play where
    show (Single (B2 card)) = "Single " ++ show card
    show (Pair (B2 card1) (B2 card2)) = "Pair " ++ show card1 ++ " " ++ show card2
    show (Triple (B2 card1) (B2 card2) (B2 card3)) = "Triple " ++ show card1 ++ " " ++ show card2 ++ " " ++ show card3
    show (Quintuple (FiveCard (B2 card1) (B2 card2) (B2 card3) (B2 card4) (B2 card5))) =
        "Quintuple " ++ show card1 ++ " " ++ show card2 ++ " " ++ show card3 ++ " " ++ show card4 ++ " " ++ show card5
    show Pass = show Pass

comparePlays :: Play -> Play -> Either ErrorMsg Ordering
comparePlays (Single fstCard) (Single sndCard) = Right $ compare fstCard sndCard
comparePlays
    (Pair fstCard1@(B2 (P.Card fstNum1 fstSuit1)) (B2 (P.Card fstNum2 fstSuit2)))
    (Pair sndCard1@(B2 (P.Card sndNum1 _)) (B2 (P.Card sndNum2 _)))
    | notPairs = Left "Not comparing 2 pairs"
    | sameNumOnBothPairs = if P.Spade `elem` [fstSuit1, fstSuit2] then Right GT else Right LT
    | otherwise = Right $ compare fstCard1 sndCard1
    where notPairs = not $ fromEnum fstNum1 == fromEnum fstNum2 && fromEnum sndNum1 == fromEnum sndNum2
          sameNumOnBothPairs = all (== fromEnum fstNum1) [fromEnum fstNum2, fromEnum sndNum1, fromEnum sndNum2]
comparePlays
    (Triple fstCard1@(B2 (P.Card fstNum1 _)) (B2 (P.Card fstNum2 _)) (B2 (P.Card fstNum3 _)))
    (Triple sndCard1@(B2 (P.Card sndNum1 _)) (B2 (P.Card sndNum2 _)) (B2 (P.Card sndNum3 _)))
    | notTriples = Left "Not comparing 2 triple"
    | otherwise = Right $ compare fstCard1 sndCard1
    where notTriples = not $ all ((== fromEnum fstNum1) . fromEnum) [fstNum2, fstNum3]
                          && all ((== fromEnum sndNum1) . fromEnum) [sndNum2, sndNum3]
comparePlays (Quintuple fstFiveCard) (Quintuple sndFiveCard) = do 
    combo1Type <- b2HandType fstFiveCard 
    combo2Type <- b2HandType sndFiveCard 
    return $ compare combo1Type combo2Type
comparePlays _ _ = Left "Only 2 play of same type can be compared"

getCardsFromPlay :: Play -> [B2Card]
getCardsFromPlay Pass = []
getCardsFromPlay (Single card1) = [card1]
getCardsFromPlay (Pair card1 card2) = [card1, card2]
getCardsFromPlay (Triple card1 card2 card3) = [card1, card2, card3]
getCardsFromPlay (Quintuple (FiveCard card1 card2 card3 card4 card5)) = [card1, card2, card3, card4, card5]

playToPlayType :: Play -> String
playToPlayType (Single {}) = "Single"
playToPlayType (Pair {}) = "Pair"
playToPlayType (Triple {}) = "Triple"
playToPlayType (Quintuple {}) = "Quintuple"
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

type Winner = Player

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
processPlay aPlayer status@(Alive _ _) Pass 
    | isNewTrick status = Left "Can't pass when starting a new trick"
    | otherwise = updateStatus aPlayer Pass status
processPlay aPlayer status@(Alive _ _) aPlay
    | not isPlayerTurn = Left $ "It's not " ++ show aPlayer ++ "'s turn"
    | otherwise = do 
        pervailingPlay  <- playOfCurrTick status
        isLegalPlayType <- (/= GT) <$> comparePlays aPlay pervailingPlay
        if isLegalPlayType 
        then updateStatus aPlayer aPlay status
        else Left $ "Attemping to play " 
                 ++ show aPlay 
                 ++ " which is not greater than the prevailing play " 
                 ++ show pervailingPlay
    where isPlayerTurn = maybe True (`isRightPlayer` aPlayer) (whoseTurn status)

updateStatus :: Player -> Play -> Status -> Either ErrorMsg Status
updateStatus _ _ Ended {} = Left gameEndedMsg
updateStatus aPlayer Pass (Alive discards hands) = Right $ Alive updatedDiscards hands
    where currTrickNo = trick . head . getDiscards $ discards
          updatedDiscards = Discards $ Discard currTrickNo Pass aPlayer : getDiscards discards
updateStatus aPlayer aPlay status@(Alive discards hands) = do
    handToUpdate <- playerToHand aPlayer hands
    updatedHand <- removePlay aPlay handToUpdate
    let updatedDiscards = Discards $ Discard trickNo aPlay aPlayer : getDiscards discards
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
                                        ++ P.cardToString (getCard card)
                                        ++ " that can't be found in the hand"

isPlayInHand :: Play -> Hand -> Maybe Bool
isPlayInHand Pass _ = Nothing
isPlayInHand (Single card1) (Hand deck) = Just $ card1 `elem` deck
isPlayInHand (Pair card1 card2) (Hand deck) = Just $ card1 `elem` deck && card2 `elem` deck
isPlayInHand (Triple card1 card2 card3) (Hand deck) = Just $ card1 `elem` deck && card2 `elem` deck && card3 `elem` deck
isPlayInHand (Quintuple (FiveCard card1 card2 card3 card4 card5)) (Hand deck) =
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

playTypeOfCurrTrick :: Status -> Either ErrorMsg String
playTypeOfCurrTrick (Alive (Discards []) _) = Left "Prevailing play type not available because starting a new game"
playTypeOfCurrTrick (Ended {}) = Left "Prevailing play type not available because game has ended"
playTypeOfCurrTrick status@(Alive (Discards discards) _) =
    if isNewTrick status 
    then Left "Prevailing play type not available because starting a new trick"  
    else Right . playToPlayType . play . head . dropWhile ((== Pass) . play) $ discards

playOfCurrTick :: Status -> Either ErrorMsg Play 
playOfCurrTick (Alive (Discards []) _) = Left "Prevailing play not available because starting a new game"
playOfCurrTick (Ended {}) = Left "Prevailing play not available because game has ended"
playOfCurrTick status@(Alive (Discards discards) _) = 
    if isNewTrick status 
    then Left "Prevailing play not available because starting a new trick" 
    else Right . play . head . dropWhile ((== Pass) . play) $ discards

newGame :: Int -> B2Deck -> Status
newGame n deck
    | n > 4 || n < 2 = error "Invalid number of players"
    | otherwise = Alive (Discards []) $ dealCard n deck

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

