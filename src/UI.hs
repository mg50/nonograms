{-# LANGUAGE TypeFamilies #-}

module UI where
import Core
import Action

class (Monad a) => UI a where
  type UIData a :: *

  initialize :: Game -> a (UIData a)
  display :: Game -> UIData a -> a ()
  promptMove :: Game -> UIData a -> a (Action, UIData a)
  shutdown :: UIData a -> a ()
