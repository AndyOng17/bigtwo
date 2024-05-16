{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (toUpper)
import Analytics
import Game
import Relude (readMaybe)

main :: IO ()
main = do
    deck <- genB2FullDeckShuffled
    let newStatus = newGame [P1,P2,P3,P4] deck
    _ <- gameLoop "New game created" newStatus []
    return ()

gameLoop :: ErrorMsg -> Status -> [Int] -> IO (Either ErrorMsg ())
gameLoop msg status selectedCards = do
    (playMade, latestSelections) <- processTurn msg status selectedCards
    print latestSelections
    case processPlay (whoseTurn status) status playMade of
        Left errMsg 
            -> gameLoop errMsg status latestSelections
        Right newStatus@(Alive (Discards (Discard _ Pass lastPlayer :_)) _)
            -> gameLoop (show lastPlayer ++ " passed") newStatus []
        Right newStatus@(Alive (Discards (Discard _ lastPlay lastPlayer:_)) _)
            -> gameLoop (show lastPlayer ++ " played " ++ show lastPlay) newStatus selectedCards
        Right newStatus@(Ended winner (Discards (Discard _ lastPlay _:_)) _) 
            -> return <$> report (show winner ++ " wins with " ++ show lastPlay) newStatus []

processTurn :: ErrorMsg -> Status -> [Int] -> IO (Play, [Int])
processTurn msg status selectedCards = do
    let ioEtAction = do 
            input <- prompt msg status selectedCards
            _     <- return $ validateInput status selectedCards (toUpper <$> input)
            return $ processInput status selectedCards (toUpper <$> input)
    etAction <- ioEtAction
    case etAction of
        Left errMsg -> processTurn errMsg status selectedCards
        Right ClearSelections -> processTurn "Selections cleared" status []
        Right (UpdateSelections newSelections) -> processTurn "Added" status newSelections
        Right (MakePlay playMade) -> return (playMade, selectedCards)

prompt :: String -> Status -> [Int] -> IO String
prompt msg status@(Alive _ _) selectedCards = report msg status selectedCards >> getLine
prompt msg status@(Ended {}) _ = print status >> putStrLn msg >> putStr "[Press any key]" >> getLine

report :: String -> Status -> [Int] -> IO ()
report msg status@(Ended {}) _ = do print status >> putStrLn msg >> putStrLn "Game ended"
report msg status@(Alive _ hands) selectedCards = do
    let currHand = (`playerToHand` hands) . whoseTurn $ status
        selectedCards' = (`inputToCard` currHand) <$> selectedCards
        prevailingPlay = either (const "New Trick") show (playOfCurrTick status)
        leaderCurrTrick = either (const "NA") show (leaderOfCurrTick status)
    print status
    putStrLn $ "Prevailing Play: [" ++ prevailingPlay ++ "] Play by: [" ++ leaderCurrTrick ++ "]"
    putStrLn msg
    putStr (show (whoseTurn status) ++ " " ++ show selectedCards' ++ " : ")

data Action = MakePlay Play | ClearSelections | UpdateSelections [Int] deriving (Eq, Show)

processInput :: Status -> [Int] -> [Char] -> Either ErrorMsg Action
processInput _ _ "P" = Right $ MakePlay Pass
processInput _ _ "C" = Right ClearSelections
processInput status@(Alive _ hands) selectedCards "S" = do
    let currPlayer = whoseTurn status
        currHand = playerToHand currPlayer hands
    selectedPlay <- inputToPlay selectedCards currHand
    return $ MakePlay selectedPlay
processInput _ selectedCards input = Right $ UpdateSelections ((read input :: Int):selectedCards)

validateInput :: Status -> [Int] -> [Char] -> Either ErrorMsg ()
validateInput (Alive {}) _ "" = Left "No input"
validateInput (Alive {}) [] "S" = Left "Can't submit when no cards are selected"
validateInput status@(Alive _ hands) selectedCards "S"
    = let currPlayer = whoseTurn status
          currHand = playerToHand currPlayer hands
      in case inputToPlay selectedCards currHand of
            (Left errMsg) -> Left errMsg
            (Right _) -> Right ()
validateInput _ _ "C" = Right ()
validateInput _ _ "P" = Right ()
validateInput status@(Alive _ hands) selectedCards input
    | not isNumber && (length input > 1 || notElem (head input) ['P', 'p', 'S', 's']) = Left "Invalid input"
    | inputAsInt `elem` selectedCards = Left "Can't select the same card twice"
    | isNumber && (inputAsInt > lengthOfHand || inputAsInt <= 0)
        = Left ("Selections available goes from 1 to " ++ show lengthOfHand)
    | isNumber && length selectedCards >= 5 = Left "Can't select more than 5 cards"
    | otherwise = Right ()
    where isNumber = case (readMaybe input :: Maybe Int) of Just _ -> True
                                                            Nothing -> False
          lengthOfHand = let (Hand aHand _) = currHand in length aHand
          currPlayer = whoseTurn status
          currHand = playerToHand currPlayer hands
          inputAsInt = read input :: Int

inputToPlay :: [Int] -> Hand -> Either ErrorMsg Play
inputToPlay [s1] (Hand aHand _) = let card = aHand !! ((read . show $ s1 :: Int) - 1) in return $ Single card
inputToPlay [s1, s2] (Hand aHand _) = let card1 = aHand !! ((read . show $ s1 :: Int) - 1)
                                          card2 = aHand !! ((read . show $ s2 :: Int) - 1)
                                      in getPlayFromCard [card1, card2]
inputToPlay [s1, s2, s3] (Hand aHand _) = let card1 = aHand !! ((read . show $ s1 :: Int) - 1)
                                              card2 = aHand !! ((read . show $ s2 :: Int) - 1)
                                              card3 = aHand !! ((read . show $ s3 :: Int) - 1)
                                          in getPlayFromCard [card1, card2, card3]
inputToPlay [s1, s2, s3, s4, s5] (Hand aHand _) = let card1 = aHand !! ((read . show $ s1 :: Int) - 1)
                                                      card2 = aHand !! ((read . show $ s2 :: Int) - 1)
                                                      card3 = aHand !! ((read . show $ s3 :: Int) - 1)
                                                      card4 = aHand !! ((read . show $ s4 :: Int) - 1)
                                                      card5 = aHand !! ((read . show $ s5 :: Int) - 1)
                                                  in getPlayFromCard [card1, card2, card3, card4, card5]
inputToPlay [_, _, _, _] _ = Left "4-card plays are illegal"

inputToCard :: Int -> Hand -> B2Card
inputToCard index (Hand aHand _) = aHand !! ((read . show $ index :: Int) - 1)
