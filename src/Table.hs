module Table where

import           Data.List

type ColSize = Int
type Style = String
type Value = String

data Cell = Cell [Style] Value

renderCell :: ColSize -> Cell -> String
renderCell colSize (Cell styles val) = startStyle ++ val ++ padding ++ endStyle
 where
  startStyle = join styles
  padding    = take (colSize - length val + 1) $ repeat ' '
  endStyle   = "\x1b[0m"

renderCols :: [[Cell]] -> [[String]]
renderCols = map (intersperse sep) . transpose . map renderCols' . transpose
 where
  getCellVal (Cell _ val) = val
  renderCols' cells = map (renderCell colSize) cells
    where colSize = maximum (map (length . getCellVal) cells)

renderRows :: [[String]] -> [String]
renderRows = map ((++ "\n") . join)

render :: [[Cell]] -> IO ()
render = putStr . join . renderRows . renderCols

-- Utils

join :: [String] -> String
join = foldr (++) ""

sep :: String
sep = renderCell 0 (ext 238 . cell $ "|")

cell :: Value -> Cell
cell val = Cell [] val

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

bgBlack = defineStyle 40 0 0
bgRed = defineStyle 41 0 0
bgGreen = defineStyle 42 0 0
bgYellow = defineStyle 43 0 0
bgBlue = defineStyle 44 0 0
bgMagenta = defineStyle 45 0 0
bgCyan = defineStyle 46 0 0
bgWhite = defineStyle 47 0 0

bgBrightBlack = defineStyle 40 1 0
bgBrightRed = defineStyle 41 1 0
bgBrightGreen = defineStyle 42 1 0
bgBrightYellow = defineStyle 43 1 0
bgBrightBlue = defineStyle 44 1 0
bgBrightMagenta = defineStyle 45 1 0
bgBrightCyan = defineStyle 46 1 0
bgBrightWhite = defineStyle 47 1 0

ext :: Int -> Cell -> Cell
ext = defineStyle 38 5
