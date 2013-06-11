module UI where
import Core

class UI a where
  initialize :: Game -> a ()
  display :: Game -> a ()
  promptGuesses :: Game -> a [(Int, Square)]
