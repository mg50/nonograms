module Game where
import Control.Monad
import Control.Monad.Trans.State
import Core
import UI
import Nonogram
import Control.Monad.Trans

data Env a = Env { ui :: a, game :: Game  }
type EnvT a = StateT (Env a) IO

uiM :: EnvT a a
uiM = liftM ui get

gameM :: EnvT a Game
gameM = liftM game get

putGame newGame = modify $ \env -> env{game=newGame}

setGameM :: Game -> EnvT a ()
setGameM g = modify $ \env -> env{game = g}

nonogramM :: EnvT a Nonogram
nonogramM = liftM nonogram gameM

pushHistory :: Clues -> EnvT a ()
pushHistory clues = modify $ \env ->
  let g = game env
      hist = history g
      newHist = clues:hist
      newGame = g{history=newHist}
  in env{game=newGame}


playSingleGame :: EnvT a ()
playSingleGame = do g <- liftM game get
                    moves <- liftIO $ promptMoves g
                    let hist = history g
                    let curr = head hist
                    let newCurr = updateClues curr
                    pushHistory newCurr

-- gameLoop = do nono <- randomNonogram 5 5
--               --putGame $ newGame nono
--               let turnLoop = do undefined
--               turnLoop
