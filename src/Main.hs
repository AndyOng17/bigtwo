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
    _ <- prompt "New game created" newStatus []
    return ()

isValidInput :: String -> Either String Char
isValidInput []   = Left "Empty input"
isValidInput (x:xs)
    | xs /= []           = Left "Incorrect size"
    | x `elem` legalChar = Right $ toUpper x
    | otherwise          = Left "Incorrect character"
    where legalChar = ['H','S','h','s','R','r']

prompt :: String -> Status -> [Int] -> IO Status
prompt msg status@(Alive _ hands) selectedCards = do
    let currHand = (`playerToHand` hands) . whoseTurn $ status
        selectedCards' = (`inputToCard` currHand) <$> selectedCards
        prevailingPlay = either (const "New Trick") show (playOfCurrTick status)
    print status
    putStrLn $ "Prevailing Play [" ++ prevailingPlay ++ "] "
    putStrLn msg
    putStr (show (whoseTurn status) ++ " " ++ show selectedCards' ++ " : ")
    newInput <- getLine
    validateInput status selectedCards (toUpper <$> newInput)
prompt msg status@(Ended {}) _ = print status >> putStrLn msg >> return status

-- validateInput should be pure - just return message, status, selectedCards
validateInput :: Status -> [Int] -> [Char] -> IO Status
validateInput status@(Alive {}) selectedCards "" = prompt "No input" status selectedCards
validateInput status@(Alive {}) [] "S" = prompt "Can't submit when no cards are selected" status []
validateInput status _ "C" = prompt "Cleared" status [] 
validateInput status@(Alive {}) _ "P"
    = let currPlayer = whoseTurn status
      in case processPlay currPlayer status Pass of
            (Left errMsg) -> prompt errMsg status []
            (Right updatedStatus) -> prompt (show currPlayer ++ " passed") updatedStatus []
validateInput status@(Alive _ hands) selectedCards "S"
    = do let currPlayer = whoseTurn status
             currHand = playerToHand currPlayer hands
             newStatus = do validatedPlay <- inputToPlay selectedCards currHand
                            newStatus' <- processPlay currPlayer status validatedPlay
                            return (newStatus', validatedPlay)
         case newStatus of (Left errMsg) -> prompt errMsg status []
                           (Right (updatedStatus, cardPlayed)) -> prompt (show cardPlayed ++ "  played ") updatedStatus []
validateInput status@(Alive _ hands) selectedCards input
    | not isNumber && (length input > 1 || notElem (head input) ['P', 'p', 'S', 's'])
        = prompt "Invalid input" status selectedCards
    | inputAsInt `elem` selectedCards
        = prompt "Can't select the same card twice" status selectedCards
    | isNumber && (inputAsInt > lengthOfHand || inputAsInt <= 0)
        = prompt ("Selections available goes from 1 to " ++ show lengthOfHand) status selectedCards
    | isNumber && length selectedCards >= 5
        = prompt "Can't select more than 5 cards" status selectedCards
    | otherwise
        = do let newSelectedCards = inputAsInt:selectedCards
             prompt "Added" status newSelectedCards
    where isNumber = case (readMaybe input :: Maybe Int) of
            Just _ -> True
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
