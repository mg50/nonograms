module UI.Console where
import UI
import Core
import qualified Data.List as L

toString :: Clue -> [String]
toString (Just Filled) = replicate 3 "####"
toString (Just Empty) = replicate 3 "...."
toString Nothing = replicate 3 "    "


newtype ConsoleUI a = ConsoleUI (IO a)

newConsoleUI :: ConsoleUI ()
newConsoleUI = ConsoleUI $ return ()

infixl 0 |>
x |> f = f x

formatRow :: [Clue] -> String
formatRow clues = clues
               |> map toString
               |> intersperse ["|", "|", "|"]
               |> L.transpose
               |> map concat
               |>

divider :: Int -> String
divider n = concat $ take n $ repeat "-"

formatClueRow :: [[Clue]] -> Int -> String
formatClueRow clues n =
  let rs = map formatRow $ clues
      rs' = map ("|" ++) rs
      rs'' = map (++ "\n") rs'
      div = divider n
  in L.intercalate div rs''


instance UI ConsoleUI where
  display game = ConsoleUI $ do
    let (Nonogram squares n) = nonogram game
    let squares' = zip squares [0..]
    let clues = last $ history game
    undefined


  getClues game = undefined
