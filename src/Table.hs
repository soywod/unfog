module Table where

import           Data.List

type Style = String
type Value = String

data Cell = Cell [Style] Value

instance Show Cell where
  show (Cell styles val) = foldr (++) "" styles ++ val

renderCell :: Int -> Cell -> String
renderCell colSize cell = startStyle ++ val ++ padding ++ endStyle
 where
  val        = getCellVal cell
  startStyle = foldr (++) "" (getCellStyles cell)
  padding    = take (colSize - length val + 1) $ repeat ' '
  endStyle   = "\x1b[0m"

renderCols :: [[Cell]] -> [[String]]
renderCols = map (intersperse sep) . transpose . map renderCols' . transpose
 where
  sep = renderCell 0 (ext 238 . cell $ "|")
  renderCols' cells = map (renderCell colSize) cells
    where colSize = maximum (map (length . getCellVal) cells)

renderRows :: [[String]] -> [String]
renderRows = map renderRows'
  where renderRows' cols = foldr (++) "" cols ++ "\n"

renderTable :: [String] -> String
renderTable = foldr (++) ""

render :: [[Cell]] -> IO ()
render = putStr . renderTable . renderRows . renderCols

-- Utils

cell :: Value -> Cell
cell val = Cell [] val

getCellVal :: Cell -> Value
getCellVal (Cell _ val) = val

getCellStyles :: Cell -> [Style]
getCellStyles (Cell styles _) = styles

defineStyle :: Int -> Int -> Int -> Cell -> Cell
defineStyle color bright shade (Cell styles val) = Cell (styles ++ [style]) val
 where
  bright' = if bright > 0 then ";" ++ show bright else ""
  shade'  = if shade > 0 then ";" ++ show shade else ""
  style   = "\x1b[" ++ show color ++ bright' ++ shade' ++ "m"

reset = defineStyle 0 0 0
bold = defineStyle 1 0 0
underline = defineStyle 4 0 0
reversed = defineStyle 7 0 0
black = defineStyle 30 0 0
red = defineStyle 31 0 0
green = defineStyle 32 0 0
yellow = defineStyle 33 0 0
blue = defineStyle 34 0 0
magenta = defineStyle 35 0 0
cyan = defineStyle 36 0 0
white = defineStyle 37 0 0

brightBlack = defineStyle 30 1 0
brightRed = defineStyle 31 1 0
brightGreen = defineStyle 32 1 0
brightyellow = defineStyle 33 1 0
brightBlue = defineStyle 34 1 0
brightMagenta = defineStyle 35 1 0
brightCyan = defineStyle 36 1 0
brightWhite = defineStyle 37 1 0

ext :: Int -> Cell -> Cell
ext = defineStyle 38 5
