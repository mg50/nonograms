{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Monad
import Control.Monad.Trans.State
import Core
import UI
import UI.Vty
import Nonogram
import Control.Monad.Trans
import Action
import System.Environment

data GameResult = Exited | Won deriving (Eq)

turnLoop :: (UI m) => Game -> UIData m -> m GameResult
turnLoop game uiData = do
  (action, uiData') <- promptMove game uiData
  case action of
    Quit -> shutdown uiData' >> return Exited
    Update sq coords -> let game' = updateGame game coords sq
                            won = gameWon game'
                        in if won
                              then shutdown uiData' >> return Won
                              else turnLoop game' uiData'
    Undo -> turnLoop (undo game) uiData'
    Redo -> turnLoop (redo game) uiData'


playGame :: (UI m) => Nonogram -> m GameResult
playGame nonogram = do let game = newGame nonogram
                       uiData <- initialize game
                       display game uiData
                       turnLoop game uiData

main :: IO ()
main = do args <- getArgs
          let [x, y] = if length args == 2
                          then map read args
                          else [10, 10]
          nono <- randomNonogram x y
          result <- unVtyIO $ playGame nono
          when (result == Won) $ putStrLn "YOU WON!!!"
