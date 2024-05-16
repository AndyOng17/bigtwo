{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- DO REMOVE to ensure that there are no incomplete pattern matches
module Game where

import qualified PokerCards as P
import Relude (maybeToRight)
import Data.List (delete, maximumBy, group)
import Data.Either.Extra (mapLeft)

type ErrorMsg = String

invalidDeckUsed :: ErrorMsg
invalidDeckUsed = "Deck with incorrect amount of cards had been used"

gameEndedMsg :: ErrorMsg
gameEndedMsg = "Plays can't be made when game has ended"


newtype B2Card = B2 { getCard :: P.Card } deriving Eq

type B2Deck = [B2Card]

instance Show B2Card where
    show (B2 card) = show card

instance Ord B2Card where
    compare (B2 (P.Card P.Two twoSuit)) (B2 (P.Card P.Two cardSuit)) = compare twoSuit cardSuit
    compare (B2 (P.Card P.Two _)) _ = GT
    compare _ (B2 (P.Card P.Two _)) = LT
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
                | Straight
                | TwoToSix
                | AceToFive
                | Flush
                | FullHouse
                | FourOfAKind
                | StraightFlush
                | RoyalFlush
                | TwoToSixFlush
                deriving (Eq, Show, Ord)

instance Enum B2HandType where
    fromEnum AceToFiveFlush = 9
    fromEnum TwoToSixFlush  = 8
    fromEnum RoyalFlush     = 7
    fromEnum StraightFlush  = 6
    fromEnum FourOfAKind    = 5
    fromEnum FullHouse      = 4
    fromEnum Flush          = 3
    fromEnum AceToFive      = 2
    fromEnum TwoToSix       = 1
    fromEnum Straight       = 0
    toEnum 9 = AceToFiveFlush
    toEnum 8 = TwoToSixFlush
    toEnum 7 = RoyalFlush
    toEnum 6 = StraightFlush
    toEnum 5 = FourOfAKind
    toEnum 4 = FullHouse
    toEnum 3 = Flush
    toEnum 2 = AceToFive
    toEnum 1 = TwoToSix
    toEnum 0 = Straight
    toEnum _ = error "Only 0 to 9 allowed for B2HandType"

b2HandType :: FiveCard -> Either ErrorMsg B2HandType
b2HandType fiveCard@(FiveCard (B2 c1) (B2 c2) (B2 c3) (B2 c4) (B2 c5))
    | isAceToFiveFlush fiveCard = Right AceToFiveFlush
    | isTwoToSixFlush fiveCard = Right TwoToSixFlush
    | isAceToFive fiveCard = Right AceToFive
    | isTwoToSix fiveCard = Right TwoToSix
    | otherwise = do
        pokerType <- maybeToRight "Cards provided does not form a valid 5-card play" $ P.handType c1 c2 c3 c4 c5
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
    (B2 (P.Card num1 suit1))
    (B2 (P.Card num2 suit2))
    (B2 (P.Card num3 suit3))
    (B2 (P.Card num4 suit4))
    (B2 (P.Card num5 suit5)))
    = all (== suit1) [suit2, suit3, suit4, suit5]
    && P.Ace `elem` comboInList
    && P.Two `elem` comboInList
    && P.Three `elem` comboInList
    && P.Four `elem` comboInList
    && P.Five `elem` comboInList
    where comboInList = [num1, num2, num3, num4, num5]

isAceToFive :: FiveCard -> Bool
isAceToFive
    (FiveCard
    (B2 (P.Card num1 suit1))
    (B2 (P.Card num2 suit2))
    (B2 (P.Card num3 suit3))
    (B2 (P.Card num4 suit4))
    (B2 (P.Card num5 suit5)))
    =  not (all (== suit1) [suit2, suit3, suit4, suit5])
    && P.Ace `elem` comboInList
    && P.Two `elem` comboInList
    && P.Three `elem` comboInList
    && P.Four `elem` comboInList
    && P.Five `elem` comboInList
    where comboInList = [num1, num2, num3, num4, num5]

isTwoToSixFlush :: FiveCard -> Bool
isTwoToSixFlush
    (FiveCard
    (B2 (P.Card num1 suit1))
    (B2 (P.Card num2 suit2))
    (B2 (P.Card num3 suit3))
    (B2 (P.Card num4 suit4))
    (B2 (P.Card num5 suit5)))
    = all (== suit1) [suit2, suit3, suit4, suit5]
    && P.Two `elem` comboInList
    && P.Three `elem` comboInList
    && P.Four `elem` comboInList
    && P.Five `elem` comboInList
    && P.Six `elem` comboInList
    where comboInList = [num1, num2, num3, num4, num5]

isTwoToSix :: FiveCard -> Bool
isTwoToSix
    (FiveCard
    (B2 (P.Card num1 suit1))
    (B2 (P.Card num2 suit2))
    (B2 (P.Card num3 suit3))
    (B2 (P.Card num4 suit4))
    (B2 (P.Card num5 suit5)))
    = not (all (== suit1) [suit2, suit3, suit4, suit5])
    && P.Two `elem` comboInList
    && P.Three `elem` comboInList
    && P.Four `elem` comboInList
    && P.Five `elem` comboInList
    && P.Six `elem` comboInList
    where comboInList = [num1, num2, num3, num4, num5]


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
    show Pass = "Pass"

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
    return $ compare combo1Type combo2Type <> compare fstFiveCard sndFiveCard
comparePlays _ _ = Left "Only 2 play of same type can be compared"

getPlayFromCard :: [B2Card] -> Either ErrorMsg Play
getPlayFromCard [c1] = return $ Single c1
getPlayFromCard [c1, c2]
    = if P.isPair (getCard c1) (getCard c2)
      then return $ Pair c1 c2
      else Left "Cards provided do not form a valid pair"
getPlayFromCard [c1, c2, c3]
    = if P.isTriple (getCard c1) (getCard c2) (getCard c3)
      then return $ Triple c1 c2 c3
      else Left "Cards provided do not form a valid triple"
getPlayFromCard [c1, c2, c3, c4, c5] = do
    let fiveCardPlay = FiveCard c1 c2 c3 c4 c5
    _ <- b2HandType fiveCardPlay
    return $ Quintuple fiveCardPlay
getPlayFromCard _ = Left "Cards provided do not form a valid play"

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


data Hand = Hand { hand :: B2Deck
                 , player :: Player }
            deriving (Show, Eq)


data Hands = FourP Hand Hand Hand Hand
           | ThreeP Hand Hand Hand
           | TwoP Hand Hand
           deriving (Eq)

instance Show Hands where
    show (FourP (Hand hand1 _) (Hand hand2 _) (Hand hand3 _) (Hand hand4 _))
        =  "Player 1: " ++ show hand1 ++ "\n"
        ++ "Player 2: " ++ show hand2 ++ "\n"
        ++ "Player 3: " ++ show hand3 ++ "\n"
        ++ "Player 4: " ++ show hand4 ++ "\n"
        ++ "           1 2 3 4 5 6 7 8 9 0 1 2 3"
    show (ThreeP (Hand hand1 _) (Hand hand2 _) (Hand hand3 _))
        =  "Player 1: " ++ show hand1 ++ "\n"
        ++ "Player 2: " ++ show hand2 ++ "\n"
        ++ "Player 3: " ++ show hand3 ++ "\n"
        ++ "           1 2 3 4 5 6 7 8 9 0 1 2 3"
    show (TwoP (Hand hand1 _) (Hand hand2 _))
        =  "Player 1: " ++ show hand1 ++ "\n"
        ++ "Player 2: " ++ show hand2 ++ "\n"
        ++ "           1 2 3 4 5 6 7 8 9 0 1 2 3"


data Seat = North | East | South | West deriving (Show, Eq, Enum)


data Player = P1 | C1 | P2 | C2 | P3 | C3 | P4 | C4 deriving (Show, Eq)


type Winner = Player


data Setting = Setting (Maybe Player) (Maybe Player) (Maybe Player) (Maybe Player)

playerToHand :: Player -> Hands -> Hand
playerToHand aPlayer = seatToHand (playerToSeat aPlayer)

seatToHand :: Seat -> Hands -> Hand
seatToHand seat (FourP north east south west) = case seat of
    North -> north
    East -> east
    South -> south
    West -> west
seatToHand seat (ThreeP north east south) = case seat of
    North -> north
    East -> east
    South -> south
    _ -> error "seatToHand: can't select West in 3-player game"
seatToHand seat (TwoP north east) = case seat of
    North -> north
    East -> east
    _ -> error "seatToHand: can't select West or South in 2-player game"

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

nextPlayer :: Hands -> Player -> Seat
nextPlayer (TwoP {}) P2 = North
nextPlayer (TwoP {}) C2 = North
nextPlayer (ThreeP {}) P3 = North
nextPlayer (ThreeP {}) C3 = North
nextPlayer (FourP {}) P4 = North
nextPlayer (FourP {}) C4 = North
nextPlayer _ currP = succ $ playerToSeat currP

isRightPlayer :: Seat -> Player -> Bool
isRightPlayer North aPlayer = aPlayer `elem` [P1, C1]
isRightPlayer East aPlayer = aPlayer `elem` [P2, C2]
isRightPlayer South aPlayer = aPlayer `elem` [P3, C3]
isRightPlayer West aPlayer = aPlayer `elem` [P4, P4]

whoseTurn :: Status -> Player
whoseTurn (Alive (Discards []) (FourP (Hand _ northP) _ _ _)) = northP
whoseTurn (Alive discards hands@(FourP hand1 hand2 hand3 hand4))
    = let latestSeat = nextPlayer hands (latestPlayer discards)
      in case latestSeat of North -> player hand1
                            East -> player hand2
                            South -> player hand3
                            West -> player hand4
whoseTurn (Alive (Discards []) (ThreeP (Hand _ northP) _ _)) = northP
whoseTurn (Alive discards hands@(ThreeP hand1 hand2 hand3))
    = let latestSeat = nextPlayer hands (latestPlayer discards)
      in case latestSeat of North -> player hand1
                            East -> player hand2
                            South -> player hand3
                            _ -> error "Corrupted status"
whoseTurn (Alive (Discards []) (TwoP (Hand _ northP) _)) = northP
whoseTurn (Alive discards hands@(TwoP hand1 hand2))
    = let latestSeat = nextPlayer hands (latestPlayer discards)
      in case latestSeat of North -> player hand1
                            East -> player hand2
                            _ -> error "Corrupted status"
whoseTurn (Ended {}) = error "whoseTurn: invoked on a game that has ended"

latestPlayer :: Discards -> Player
latestPlayer = playedBy . head . getDiscards

getNoOfPlayers :: Hands -> Int
getNoOfPlayers (FourP {}) = 4
getNoOfPlayers (ThreeP {}) = 3
getNoOfPlayers (TwoP {}) = 2

processPlay :: Player -> Status -> Play -> Either ErrorMsg Status
processPlay _ (Ended {}) _ = Left gameEndedMsg
processPlay aPlayer status@(Alive (Discards discards) _) Pass
    | null discards = Left "North can't pass when starting a new game"
    | isNewTrick status = Left "Can't pass when starting a new trick"
    | otherwise = updateStatus aPlayer Pass status
processPlay aPlayer status@(Alive (Discards []) _) fstPlay = updateStatus aPlayer fstPlay status
processPlay aPlayer status@(Alive _ _) aPlay
    | not isPlayerTurn = Left $ "It's not " ++ show aPlayer ++ "'s turn"
    | isNewTrick status = updateStatus aPlayer aPlay status
    | otherwise = do
        pervailingPlay  <- playOfCurrTick status
        isLegalPlayType <- (== GT) <$> mapLeft (wrongPlayType pervailingPlay) (comparePlays aPlay pervailingPlay)
        if isLegalPlayType
        then updateStatus aPlayer aPlay status
        else Left $ "Attempting to play "
                 ++ show aPlay
                 ++ " which is not greater than the prevailing play "
                 ++ show pervailingPlay
    where isPlayerTurn = whoseTurn status == aPlayer
          wrongPlayType pervailPlay = const 
                                    $ "Attempting to play " 
                                   ++ show aPlay 
                                   ++ " when the prevailing play is " 
                                   ++ show pervailPlay

updateStatus :: Player -> Play -> Status -> Either ErrorMsg Status
updateStatus _ _ Ended {} = Left gameEndedMsg
updateStatus aPlayer Pass (Alive discards hands) = Right $ Alive updatedDiscards hands
    where currTrickNo = trick . head . getDiscards $ discards
          updatedDiscards = Discards $ Discard currTrickNo Pass aPlayer : getDiscards discards
updateStatus aPlayer aPlay status@(Alive discards hands) = do
    updatedHand <- removePlay aPlay (playerToHand aPlayer hands)
    let updatedDiscards = Discards $ Discard trickNo aPlay aPlayer : getDiscards discards
        isUpdatedHandEmpty = null $ hand updatedHand
    if isUpdatedHandEmpty
    then updateHands (playerToSeat aPlayer) updatedHand hands >>= (return . Ended aPlayer updatedDiscards)
    else updateHands (playerToSeat aPlayer) updatedHand hands >>= (return . Alive updatedDiscards)
    where isNewGame = null $ getDiscards discards
          isStartingNewTrick = isNewTrick status
          trickNo | isNewGame = 1
                  | isStartingNewTrick = (+1) . trick . head . getDiscards $ discards
                  | otherwise = trick . head . getDiscards $ discards

updateHands :: Seat -> Hand -> Hands -> Either ErrorMsg Hands
updateHands seat aHand (FourP north east south west) = case seat of
    North -> Right $ FourP aHand east south west
    East -> Right $ FourP north aHand south west
    South -> Right $ FourP north east aHand west
    West -> Right $ FourP north east south aHand
updateHands seat aHand (ThreeP north east south) = case seat of
    North -> Right $ ThreeP aHand east south
    East -> Right $ ThreeP north aHand south
    South -> Right $ ThreeP north east aHand
    _ -> Left $ "Seat " ++ show seat ++ " is not in play"
updateHands seat aHand (TwoP north east) = case seat of
    North -> Right $ TwoP aHand east
    East -> Right $ TwoP north aHand
    _ -> Left $ "Seat " ++ show seat ++ " is not in play"

removePlay :: Play -> Hand -> Either ErrorMsg Hand
removePlay Pass _ = Left "'Pass' can't be removed from a hand"
removePlay aPlay aHand = foldr removeCard' (Right aHand) (getCardsFromPlay aPlay)
    where removeCard' cardToRemove fromHand = fromHand >>= removeCard cardToRemove

removeCard :: B2Card -> Hand -> Either ErrorMsg Hand
removeCard _ (Hand [] _) = Left "Empty hand"
removeCard card (Hand aHand aPlayer) = if card `elem` aHand
                                then Right . (`Hand` aPlayer) $ delete card aHand
                                else Left $ "Attempting to remove "
                                         ++ P.cardToString (getCard card)
                                         ++ " that can't be found in the hand"

isPlayInHand :: Play -> Hand -> Maybe Bool
isPlayInHand Pass _ = Nothing
isPlayInHand (Single card1) (Hand deck _) = Just $ card1 `elem` deck
isPlayInHand (Pair card1 card2) (Hand deck _) = Just $ card1 `elem` deck && card2 `elem` deck
isPlayInHand (Triple card1 card2 card3) (Hand deck _) = Just $ card1 `elem` deck && card2 `elem` deck && card3 `elem` deck
isPlayInHand (Quintuple (FiveCard card1 card2 card3 card4 card5)) (Hand deck _) =
    Just $ card1 `elem` deck && card2 `elem` deck && card3 `elem` deck && card4 `elem` deck && card5 `elem` deck

isNewTrick :: Status -> Bool
isNewTrick (Ended {}) = False
isNewTrick (Alive (Discards []) _) = True
isNewTrick (Alive (Discards discards) FourP {})
    | length discards < 4 = False
    | otherwise = let (lastPlay1:lastPlay2:lastPlay3:_) = discards
                  in play lastPlay1 == Pass && play lastPlay2 == Pass && play lastPlay3 == Pass
isNewTrick (Alive (Discards discards) ThreeP {})
    | length discards < 3 = False
    | otherwise = let (lastPlay1:lastPlay2:_) = discards
                  in play lastPlay1 == Pass && play lastPlay2 == Pass
isNewTrick (Alive (Discards discards) TwoP {})
    | length discards < 2 = False
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

leaderOfCurrTick :: Status -> Either ErrorMsg Player
leaderOfCurrTick (Alive (Discards []) _) = Left "Prevailing play not available because starting a new game"
leaderOfCurrTick (Ended {}) = Left "Prevailing play not available because game has ended"
leaderOfCurrTick status@(Alive (Discards discards) _) =
    if isNewTrick status
    then Left "Prevailing play not available because starting a new trick"
    else Right . playedBy . head . dropWhile ((== Pass) . play) $ discards

newGame :: [Player] -> B2Deck -> Status
newGame [northP, eastP, southP, westP] deck
    = Alive (Discards []) $ dealCard (FourP (Hand [] northP) (Hand [] eastP) (Hand [] southP) (Hand [] westP)) deck
newGame [northP, eastP, southP] deck
    = Alive (Discards []) $ dealCard (ThreeP (Hand [] northP) (Hand [] eastP) (Hand [] southP)) deck
newGame [northP, eastP] deck
    = Alive (Discards []) $ dealCard (TwoP (Hand [] northP) (Hand [] eastP)) deck
newGame _ _ = error "Invalid number of players"

dealCard :: Hands -> B2Deck -> Hands
dealCard _ deck | length deck /= 52 = error "Deck with incorrect amount of cards had been used"
dealCard fourP@(FourP {}) deck = dealCards' deck fourP
dealCard threeP@(ThreeP {}) deck = dealCards' deck threeP
dealCard twoP@(TwoP {}) deck = dealCards' deck twoP

dealCards' :: B2Deck -> Hands -> Hands
dealCards' deck hands@(FourP (Hand hand1 northP) (Hand hand2 eastP) (Hand hand3 southP) (Hand hand4 westP)) = case splitAt 4 deck of
    ([], _) -> hands
    ([card1, card2, card3, card4], restOfDeck) ->
        dealCards' restOfDeck $ FourP (Hand (card1:hand1) northP)
                                      (Hand (card2:hand2) eastP)
                                      (Hand (card3:hand3) southP)
                                      (Hand (card4:hand4) westP)
    _ -> error "Deck with incorrect amount of cards had been used"
dealCards' deck (ThreeP (Hand hand1 northP) h2@(Hand hand2 eastP) h3@(Hand hand3 southP)) = case splitAt 3 deck of
    ([lastCard], _) -> ThreeP (Hand (lastCard:hand1) northP) h2 h3
    ([card1, card2, card3], restOfDeck) ->
        dealCards' restOfDeck $ ThreeP (Hand (card1:hand1) northP)
                                       (Hand (card2:hand2) eastP)
                                       (Hand (card3:hand3) southP)
    _ -> error "Deck with incorrect amount of cards had been used"
dealCards' deck hands@(TwoP (Hand hand1 northP) (Hand hand2 eastP)) = case splitAt 2 deck of
    ([], _) -> hands
    ([card1, card2], restOfDeck) ->
        dealCards' restOfDeck $ TwoP (Hand (card1:hand1) northP)
                                     (Hand (card2:hand2) eastP)
    _ -> error "Deck with incorrect amount of cards had been used"
