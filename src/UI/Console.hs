{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI.Console where
import UI
import Core
import qualified Data.List as L
import qualified Data.Map as M

import Control.Monad.Trans

toString :: Guess -> String
toString (Just Filled) = "##"
toString (Just Empty) = ".."
toString Nothing = "  "


newtype ConsoleUI a = ConsoleUI { runConsole :: IO a } deriving (Monad)

instance MonadIO ConsoleUI where
  liftIO = ConsoleUI

newConsoleUI :: ConsoleUI ()
newConsoleUI = ConsoleUI $ return ()

formatRow :: [Guess] -> [Int] -> String
formatRow guesses clues = map toString guesses
                       |> L.intercalate "|"
                       |> (" |" ++ )
                       |> (numbers ++)
  where numbers = L.intercalate " " $ map show clues


divider :: Int -> String
divider n = concat $ take (3*n - 1) $ repeat "-"

guessRows :: Nonogram -> Guesses -> [[Guess]]
guessRows (Nonogram squares n) guesses =
  let guesses' = take (length squares) $ map (flip M.lookup guesses) [0..]
  in rows guesses' n


pad :: [String] -> [String]
pad strs = map (padOne len) strs
  where len = maximum $ map length strs
        padOne n str = padding n (length str) ++ str
        padding n m = concat $ replicate (n - m) " "

formatGame :: Game -> String
formatGame (Game nono []) = error "empty history"
formatGame (Game nono@(Nonogram _ n) hist) =
  current |> guessRows nono
          |> zip (map clues $ nonogramRows nono)
          |> map (\(clues, row) -> formatRow row clues)
          |> L.intersperse ("|" ++ divider n)
          |> (["|" ++ divider n] ++)
          |> (++ ["|" ++ divider n])
          |> pad
          |> map (++ "|")
          |> L.intercalate "\n"

  where current = head hist


instance UI ConsoleUI where
  display game = ConsoleUI $ putStrLn (formatGame game)
  promptGuesses game = ConsoleUI $ do move <- getLine
                                      return [(read move, Just Filled)]

-- display (Game (Nonogram _ n) hist) = ConsoleUI $ do
--   let curr = head hist
--   let rs = rows nono
--   let



-- formatRows :: [[Guess]] -> [[Int]] -> String
-- formatRows guesses clues =
--   let nums' = map show clues
--            |> L.intercalate " "
--            |> pad

--       pad xs = let n = maximum (map length xs) in map (padOne n) xs
--       padOne n x = let l = n - length x in replicate l " " ++ x

--   in undefined


-- formatClueRow :: [[Guess]] -> Int -> String
-- formatClueRow guesses clues =
--   let rs = map formatRow $ clues
--       rs' = map ("|" ++) rs
--       rs'' = map (++ "\n") rs'
--       div = divider n
--   in L.intercalate div rs''


-- instance UI ConsoleUI where
--   display game = ConsoleUI $ do
--     let (Nonogram squares n) = nonogram game
--     let squares' = zip squares [0..]
--     let clues = last $ history game
--     undefined


--   getClues game = undefined
