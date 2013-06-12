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

type EnvT ui = StateT Game ui

-- pushHistory :: (Monad ui) => Guesses -> EnvT ui ()
-- pushHistory guesses = modify $ \g -> let newHist = g |> history |> (guesses:)
--                                      in g{history=newHist}


-- turn :: (Monad ui, UI ui) => EnvT ui Guesses
-- turn = do g <- get
--           lift $ display g
--           moves <- lift $ promptGuesses g
--           let hist = history g
--                   |> head
--                   |> (`updateGuesses` moves)
--           pushHistory hist
--           return hist

-- playGame :: (MonadIO ui, Monad ui, UI ui) => EnvT ui ()
-- playGame = do guesses <- turn
--               game <- get
--               let nono = nonogram game
--               if wins nono guesses
--                  then return ()
--                  else playGame

-- main :: IO ()
-- main = do rand <- randomNonogram 5 5
--           runCursorM $ runStateT (playGame :: EnvT ConsoleM ()) $ newGame rand
--           return ()

turnLoop game uiData = do
  (action, uiData') <- unVtyIO $ promptGuesses game uiData
  case action of
    Quit -> unVtyIO $ shutdown uiData'
    Update sq coords -> let game' = updateGame game coords sq
                            won = gameWon game'
                        in if won
                              then do unVtyIO $ shutdown uiData'
                                      putStrLn "YOU WIN!!!!"
                              else turnLoop game' uiData'
    Undo -> turnLoop (undo game) uiData'
    Redo -> turnLoop (redo game) uiData'


main = do game <- liftM newGame $ randomNonogram 5 5
          vtyData <- unVtyIO $ initialize game
          unVtyIO $ display game vtyData
          turnLoop game vtyData
