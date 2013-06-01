module UI where
import Core

class UI a where
  display :: Game -> a ()
  getClues :: Game -> a [(Int, Square)]
  promptMoves :: Game -> a [(Int, Clue)]
