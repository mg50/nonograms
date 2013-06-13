module Action where
import Core

data Action = Quit | Update Square [(Int, Int)] | Undo | Redo
