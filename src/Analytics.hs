{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- DO REMOVE TO CHECK if there are incomplete pattern-matchings
module Analytics where

import Game
import qualified Data.List.Duplicate as LD
import qualified Data.Set as Set
import System.Random (newStdGen)
import RandomCard (genFullDeckShuffled)
import Data.List (partition, subsequences, sort, nub, minimumBy, find, intersect)
import qualified PokerCards as P
import PokerCards
import Data.Foldable (Foldable(fold))
import Control.Monad (join)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Printf (printf)
import Data.Monoid (All(All, getAll))

genB2FullDeckShuffled :: IO B2Deck
genB2FullDeckShuffled = (fmap . fmap) B2 ( newStdGen >>= return . genFullDeckShuffled )

analyzeHand :: Hand -> [[B2Card]]
analyzeHand = undefined

runAnalytics :: IO ()
runAnalytics = do
    shuffledDeck <- genB2FullDeckShuffled
    let (FourP hand1 hand2 hand3 hand4) = dealCard (FourP (Hand [] P1) (Hand [] P2) (Hand [] P3) (Hand [] P4)) shuffledDeck

    putStrLn "Hand 1 Analytics:"
    print hand1
    putStr "Pairs " <> print (allPairs hand1)
    putStr "Triples " <> print (bestTriples hand1)
    putStr "Quads " <> print (allQuads hand1)
    putStr "Full Houses " <> print (bestFullHouses hand1)
    putStr "Royal Flushes " <> print (allRoyalFlushes hand1)
    putStr "Straight Flushes " <> print (allStraightFlushes hand1)
    putStr "Straights " <> print (bestStraights hand1)
    putStr "Flushes " <> print (bestFlushes hand1)
    putStrLn "Hand 2 Analytics:"
    print hand2
    putStr "Pairs " <> print (allPairs hand2)
    putStr "Triples " <> print (bestTriples hand2)
    putStr "Quads " <> print (allQuads hand2)
    putStr "Full Houses " <> print (bestFullHouses hand2)
    putStr "Royal Flushes " <> print (allRoyalFlushes hand2)
    putStr "Straight Flushes " <> print (allStraightFlushes hand2)
    putStr "Straights " <> print (bestStraights hand2)
    putStr "Flushes " <> print (bestFlushes hand2)
    putStrLn "Hand 3 Analytics:"
    print hand3
    putStr "Pairs " <> print (allPairs hand3)
    putStr "Triples " <> print (bestTriples hand3)
    putStr "Quads " <> print (allQuads hand3)
    putStr "Full Houses " <> print (bestFullHouses hand3)
    putStr "Royal Flushes " <> print (allRoyalFlushes hand3)
    putStr "Straight Flushes " <> print (allStraightFlushes hand3)
    putStr "Straights " <> print (bestStraights hand3)
    putStr "Flushes " <> print (bestFlushes hand3)
    putStrLn "Hand 4 Analytics:"
    print hand4
    putStr "Pairs " <> print (allPairs hand4)
    putStr "Triples " <> print (bestTriples hand4)
    putStr "Quads " <> print (allQuads hand4)
    putStr "Full Houses " <> print (bestFullHouses hand4)
    putStr "Royal Flushes " <> print (allRoyalFlushes hand4)
    putStr "Straight Flushes " <> print (allStraightFlushes hand4)
    putStr "Straights " <> print (bestStraights hand4)
    putStr "Flushes " <> print (bestFlushes hand4)
    return ()

playStats :: Float -> IO ()
playStats n = do
    putStr "\n"
    let noOfGames = printf "%.0f" n :: String
        underline = replicate (25 + length noOfGames) '='
    putStrLn ("Play Statistics on " ++ noOfGames ++ " games")
    putStrLn underline
    putStr "\n"
    (totalPairs, totalTriples, totalQuads, totalFullHouses, totalRoyalFlushes, totalStraightFlushes, totalStraights, totalFlushes)
        <- playStats' n (0, 0, 0, 0, 0, 0, 0, 0)
    putStrLn "Play Category\t\t\tTotal\tPer Game"
    putStrLn "-------------\t\t\t-----\t--------"
    printf "Total Pairs:\t\t\t%.0f\t%.4f\n" totalPairs (totalPairs / n)
    printf "Total Triples:\t\t\t%.0f\t%.4f\n" totalTriples (totalTriples / n)
    printf "Total Quads:\t\t\t%.0f\t%.4f\n" totalQuads (totalQuads / n)
    printf "Total Full Houses:\t\t%.0f\t%.4f\n" totalFullHouses (totalFullHouses / n)
    printf "Total Royal Flushes:\t\t%.0f\t%.4f\n" totalRoyalFlushes (totalRoyalFlushes / n)
    printf "Total Straight Flushes:\t\t%.0f\t%.4f\n" totalStraightFlushes (totalStraightFlushes / n)
    printf "Total Straight:\t\t\t%.0f\t%.4f\n" totalStraights (totalStraights / n)
    printf "Total Flushes:\t\t\t%.0f\t%.4f\n" totalFlushes (totalFlushes / n)

playStats' :: Float -> (Float, Float, Float, Float, Float, Float, Float, Float) -> IO (Float, Float, Float, Float, Float, Float, Float, Float)
playStats' 0 totals = return totals
playStats' n
    ( totalPairs
    , totalTriples
    , totalQuads
    , totalFullHouses
    , totalRoyalFlushes
    , totalStraightFlushes
    , totalStraights
    , totalFlushes) = do
        shuffledDeck <- genB2FullDeckShuffled
        let (FourP hand1 hand2 hand3 hand4) = dealCard (FourP (Hand [] P1) (Hand [] P2) (Hand [] P3) (Hand [] P4)) shuffledDeck
            newTotalPairs           = totalPairs           + sum (fromIntegral . length . allPairs           <$> [hand1, hand2, hand3, hand4])
            newTotalTriples         = totalTriples         + sum (fromIntegral . length . bestTriples        <$> [hand1, hand2, hand3, hand4])
            newTotalQuads           = totalQuads           + sum (fromIntegral . length . allQuads           <$> [hand1, hand2, hand3, hand4])
            newTotalFullHouses      = totalFullHouses      + sum (fromIntegral . length . bestFullHouses     <$> [hand1, hand2, hand3, hand4])
            newTotalRoyalFlushes    = totalRoyalFlushes    + sum (fromIntegral . length . allRoyalFlushes    <$> [hand1, hand2, hand3, hand4])
            newTotalStraightFlushes = totalStraightFlushes + sum (fromIntegral . length . allStraightFlushes <$> [hand1, hand2, hand3, hand4])
            newTotalStraights       = totalStraights       + sum (fromIntegral . length . bestStraights      <$> [hand1, hand2, hand3, hand4])
            newTotalFlushes         = totalFlushes         + sum (fromIntegral . length . bestFlushes        <$> [hand1, hand2, hand3, hand4])
        playStats' (n - 1) ( newTotalPairs
                           , newTotalTriples
                           , newTotalQuads
                           , newTotalFullHouses
                           , newTotalRoyalFlushes
                           , newTotalStraightFlushes
                           , newTotalStraights
                           , newTotalFlushes)

sampleHand :: Hand
sampleHand = Hand [ B2 (P.Card P.Three Spade)
                  , B2 (P.Card P.Four  Heart)
                  , B2 (P.Card P.Five  Club)
                  , B2 (P.Card P.Six   Diamond)
                  , B2 (P.Card P.Seven Spade)
                  , B2 (P.Card P.Eight Heart)
                  , B2 (P.Card P.Nine  Club)
                  , B2 (P.Card P.Ten   Diamond)
                  , B2 (P.Card P.Jack  Spade)
                  , B2 (P.Card P.Jack  Club)
                  , B2 (P.Card P.Jack  Diamond)
                  , B2 (P.Card P.Queen Club)
                  , B2 (P.Card P.Ace   Spade)] P1


overlapHand :: Hand
overlapHand = Hand [ B2 (P.Card P.Three Diamond)
                   , B2 (P.Card P.Four  Diamond)
                   , B2 (P.Card P.Five  Diamond)
                   , B2 (P.Card P.Six   Diamond)
                   , B2 (P.Card P.Seven Diamond)
                   , B2 (P.Card P.Eight Diamond)
                   , B2 (P.Card P.Nine  Diamond)
                   , B2 (P.Card P.Ten Diamond)
                   , B2 (P.Card P.Jack Diamond)
                   , B2 (P.Card P.Jack  Club)
                   , B2 (P.Card P.Queen  Diamond)
                   , B2 (P.Card P.King Diamond)
                   , B2 (P.Card P.Ace   Diamond)] P1

runOverflow :: IO ()
runOverflow = do
    let aHand = Hand [ B2 (Card Five Spade)
                    , B2 (Card Two Spade)
                    , B2 (Card Five Diamond)
                    , B2 (Card Nine Diamond)
                    , B2 (Card Eight Spade)
                    , B2 (Card Nine Spade)
                    , B2 (Card Two Heart)
                    , B2 (Card Seven Spade)
                    , B2 (Card Three Club)
                    , B2 (Card Seven Club)
                    , B2 (Card Six Spade)
                    , B2 (Card Ten Heart)
                    , B2 (Card Ace Heart)] P1
    putStrLn "Problematic Hand:"
    print aHand
    putStr "Pairs " <> print (allPairs aHand)
    putStr "Triples " <> print (bestTriples aHand)
    putStr "Quads " <> print (allQuads aHand)
    putStr "Straights " <> print (allStraights aHand)

allPairs :: Hand -> [[B2Card]]
allPairs (Hand aHand _) = do
    oneSet <- sameRankCards
    convertToPair oneSet
    where sameRankCards = filter ((>= 2) . length) $ LD.groupBy rankOrder aHand
          convertToPair b2Cards = case b2Cards of
            [c1, c2, c3, c4] -> [[c1, c2], [c3, c4]]
            [c1, c2, _] -> if P.Spade `elem` (P.suit . getCard <$> b2Cards)
                           then [snd $ partition ((== P.Spade) . P.suit . getCard) b2Cards]
                           else [[c1, c2]]
            [c1, c2] -> [[c1, c2]]
            [_] -> error "Expect list of 2-4 cards of the same rank"
            _   -> error "5 cards of identical rank found"

allTriples :: Hand -> [[B2Card]]
allTriples (Hand aHand _) = filter ((== 3) . length) $ LD.groupBy rankOrder (sort aHand)

bestTriples :: Hand -> [[B2Card]]
bestTriples (Hand aHand _) =  filter triple
                           .  nub
                           .  sort
                          <$> filter ((== 3) . length)
                           $  subsequences aHand
    where triple [B2 card1, B2 card2, B2 card3] = P.isTriple card1 card2 card3
          triple _ = error "Can only process 3-card plays"

allQuads :: Hand -> [[B2Card]]
allQuads (Hand aHand _) = filter ((== 4) . length) $ LD.groupBy rankOrder (sort aHand)

allFullHouses :: Hand -> [[B2Card]]
allFullHouses = find5CardPlays P.isFullHouse

bestFullHouses :: Hand -> [[B2Card]]
bestFullHouses aHand = mapMaybe selBestFullHouse fullHousesGrped
    where rankOfFullHouse :: [B2Card] -> Maybe P.Number
          rankOfFullHouse fullHouse@[_,_,_,_,_] = listToMaybe . fmap (number . getCard) $ getTriple fullHouse
          rankOfFullHouse _ = error "Can only process 5-card plays that are full houses"

          getTriple :: [B2Card] -> [B2Card]
          getTriple fullHouse@[_,_,_,_,_] = join . filter ((== 3) . length) $ LD.groupBy rankOrder fullHouse
          getTriple _ = error "Can only process 5-card plays that are full houses"

          getPair :: [B2Card] -> [B2Card]
          getPair fullHouse@[_,_,_,_,_] = join . filter ((== 2) . length) $ LD.groupBy rankOrder fullHouse
          getPair _ = error "Can only process 5-card plays that are full houses"

          compareFullHouses :: [B2Card] -> [B2Card] -> Ordering
          compareFullHouses fullHouse1 fullHouse2 = compare (rankOfFullHouse fullHouse1) (rankOfFullHouse fullHouse2)

          fullHousesGrped :: [[[B2Card]]]
          fullHousesGrped = LD.groupBy compareFullHouses (allFullHouses aHand)

          selBestFullHouse :: [[B2Card]] -> Maybe [B2Card]
          selBestFullHouse fullHousesOfIdenticalRanks = find ((== weakestPair) . getPair) fullHousesOfIdenticalRanks
            where weakestPair = minimum (sort . getPair <$> fullHousesOfIdenticalRanks)

allStraights :: Hand -> [[B2Card]]
allStraights = fmap selBestStraight
              . LD.groupBy grpStraightsByRank
              . find5CardPlays P.isStraight
    where grpStraightsByRank :: [B2Card] -> [B2Card] -> Ordering
          grpStraightsByRank straight1 straight2 = fold $ zipWith compareRanks straight1 straight2
              where compareRanks card1 card2 = compare (number . getCard $ card1) (number . getCard $ card2)

          selBestStraight :: [[B2Card]] -> [B2Card]
          selBestStraight straightsOfIdenticalRanks = minimumBy straightsWithWeakestBottomCards straightsWithStrongestTopCard
              where straightsOfIdenticalRanksSorted = sort <$> straightsOfIdenticalRanks
                    strongestTopCard = maximum . fmap (!! 4) $ straightsOfIdenticalRanksSorted
                    straightsWithStrongestTopCard =
                      filter ((== strongestTopCard) . (!! 4)) straightsOfIdenticalRanksSorted
                    straightsWithWeakestBottomCards
                      [fstPlayC1, fstPlayC2, fstPlayC3, fstPlayC4, _]
                      [sndPlayC1, sndPlayC2, sndPlayC3, sndPlayC4, _]
                          = let allComparisons = [ compare fstPlayC1 sndPlayC1
                                                 , compare fstPlayC2 sndPlayC2
                                                 , compare fstPlayC3 sndPlayC3
                                                 , compare fstPlayC4 sndPlayC4 ]
                                (strongerCards, weakerCards) = partition (== GT) . filter (`elem` [GT, LT]) $ allComparisons
                            in compare (length strongerCards) (length weakerCards)
                    straightsWithWeakestBottomCards _ _ = error "Can only process 5-card plays that are straights"

bestStraights :: Hand -> [[B2Card]]
bestStraights aHand = resolveOverLaps allPossibleStraights []
    where ranksOfAllPairs = nub (fmap (number . getCard) =<< allPairs aHand)
          allPossibleStraights = allStraights aHand
          isOverlap straight1 straight2 = not . getAll . foldMap (All . flip elem ranksOfAllPairs)
                                        $ (number . getCard <$> straight1) `intersect` (number . getCard <$> straight2)
          resolveOverLaps :: [[B2Card]] -> [[B2Card]] -> [[B2Card]]
          resolveOverLaps [] selectedStraights = selectedStraights
          resolveOverLaps (straight:otherStraights) [] = resolveOverLaps otherStraights [straight]
          resolveOverLaps (straight:otherStraights) [selectedStraight]
            = if isOverlap straight selectedStraight
              then resolveOverLaps otherStraights [min selectedStraight straight]
              else resolveOverLaps otherStraights [selectedStraight, straight]
          resolveOverLaps (straight:otherStraights) [selectedStraight1, selectedStraight2] = do
            case (isOverlap straight selectedStraight1, isOverlap straight selectedStraight2) of
                (False, False) -> error "No of independent straights in a hand can't exceed 2"
                (True, False) -> resolveOverLaps otherStraights [min straight selectedStraight1, selectedStraight2]
                (False, True) -> resolveOverLaps otherStraights [min straight selectedStraight2, selectedStraight1]
                (True, True) -> case (compare straight selectedStraight1, compare straight selectedStraight2) of
                                    (GT, GT) -> let smallerOfTwo = min selectedStraight1 selectedStraight2
                                                in resolveOverLaps otherStraights [straight, smallerOfTwo]
                                    (LT, LT) -> let biggerOfTwo = max selectedStraight1 selectedStraight2
                                                in resolveOverLaps otherStraights [straight, biggerOfTwo]
                                    _ -> resolveOverLaps otherStraights [selectedStraight1, selectedStraight2]
          resolveOverLaps _ _ = error "No of independent straights in a hand can't exceed 2"

allFlushes :: Hand -> [[B2Card]]
allFlushes = find5CardPlays P.isFlush

bestFlushes :: Hand -> [[B2Card]]
bestFlushes (Hand aHand _) = do
    cardsOfSameSuit <- suitsWith5OrMoreCards
    let selCards | length cardsOfSameSuit < 10 = [take 5 . sort $ cardsOfSameSuit]
                 | length cardsOfSameSuit >= 10 = let (fst5, theRest) = splitAt 5 . sort $ cardsOfSameSuit
                                                  in [fst5, take 5 theRest]
                 | otherwise = error "bestFlushes: Unexpected error"
    selCards
    where compareSuit card1 card2 = compare (suit . getCard $ card1) (suit . getCard $ card2)
          suitsWith5OrMoreCards = filter ((>= 5) . length) $ LD.groupBy compareSuit aHand 

allStraightFlushes :: Hand -> [[B2Card]]
allStraightFlushes = find5CardPlays P.isStraightFlush

allRoyalFlushes :: Hand -> [[B2Card]]
allRoyalFlushes = find5CardPlays P.isRoyalFlush

find5CardPlays :: (Card -> Card -> Card -> Card -> Card -> Bool) -> Hand -> [[B2Card]]
find5CardPlays verifyingFunc (Hand aHand _) = filter (isPlay verifyingFunc)
                                           . nub
                                           . fmap sort
                                           . filter ((== 5) . length)
                                           $ subsequences aHand

isPlay :: (Card -> Card -> Card -> Card -> Card -> Bool) -> [B2Card] -> Bool
isPlay verifyingFunc fiveCard = case getCard <$> fiveCard of
                                    [c1,c2,c3,c4,c5] -> verifyingFunc c1 c2 c3 c4 c5
                                    _ -> error "Can only evaluate 5-card plays"

rankOrder :: B2Card -> B2Card -> Ordering
rankOrder (B2 (P.Card num1 _)) (B2 (P.Card num2 _)) = compare num1 num2

hasDuplicates :: [B2Card] -> Bool
hasDuplicates list = length rankList /= length set
  where rankList = number . getCard <$> list
        set = Set.fromList rankList
