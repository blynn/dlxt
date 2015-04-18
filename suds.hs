import Control.Applicative
import Data.Char
import Data.List
import Data.List.Split
import DLX

main = getContents
  >>= putStr . sudoku . take 81 . filter (\c -> c == '.' || isDigit c)

b9 a b c = 81*a + 9*b + c
con r c d = [b9 0 r c, b9 1 r d, b9 2 c d, b9 3 ((r`div`3)*3+(c`div`3)) d]
ds = [0..8]
sudoku inp = let
  board = (\(ch, n) -> 9*n + digitToInt ch - 1)
    <$> filter (isDigit . fst) (zip inp [0..])
  in unlines . chunksOf 9 . (intToDigit . (+1) . (`mod` 9) <$>)
    . sort $ board ++ (head $ dlxSolve (con <$> ds <*> ds <*> ds) board)
