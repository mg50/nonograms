{-# LANGUAGE TypeFamilies #-}

module UI where
import Core
import Action

class UI a where
  type UIData a :: *

  initialize :: Game -> a (UIData a)
  display :: Game -> UIData a -> a ()
  promptGuesses :: Game -> UIData a -> a (Action, UIData a)
  toIO :: a b -> IO b
  shutdown :: UIData a -> a ()
