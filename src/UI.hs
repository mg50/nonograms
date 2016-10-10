{-# LANGUAGE TypeFamilies #-}

module UI where
import Core
import Action

class (Monad a) => UI a where
  type UIChannel a :: *

  initialize :: Game -> a (UIChannel a)
  display :: Game -> UIChannel a -> a ()
  promptAction :: Game -> UIChannel a -> a Action
  shutdown :: UIChannel a -> a ()
