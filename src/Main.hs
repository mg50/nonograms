module Main where
import Control.Monad
import Core
import UI
import UI.Vty
import Nonogram
import Control.Monad.Trans
import Action
import System.Environment

data GameResult = Exited | Won deriving (Eq)

turnLoop :: (UI m) => Game -> UIChannel m -> m GameResult
turnLoop game uiChannel = do
  display game uiChannel
  action <- promptAction game uiChannel
  case action of
    Quit -> shutdown uiChannel >> return Exited
    Update sq coords -> let game' = updateGame game coords sq
                        in if gameWon game'
                              then shutdown uiChannel >> return Won
                              else turnLoop game' uiChannel
    Undo -> turnLoop (undo game) uiChannel
    Redo -> turnLoop (redo game) uiChannel

playGame :: (UI m) => Nonogram -> m GameResult
playGame nonogram = do let game = newGame nonogram
                       uiChannel <- initialize game
                       turnLoop game uiChannel

main :: IO ()
main = do args <- getArgs
          let [x, y] = if length args == 2
                          then map read args
                          else [10, 10]
          nono <- randomNonogram x y
          result <- unVtyIO $ playGame nono
          when (result == Won) $ putStrLn "YOU WON!!!"
