-- Prints the number of solutions to tiling a 6x10 board with the 12 free
-- pentominoes by finding them all. There are 9356 / 4 = 2339 solutions,
-- where we consider reflections and rotations of a solution to be equivalent.

import Control.Applicative
import Data.List
import DLX

canon poly = let xmin = minimum $ fst <$> poly in
  sort $ map (\(x, y) -> (x - xmin, y)) poly

ds = [(0, 1), (1, 0), (0, -1), (-1, 0)]

adj (x, y) banned =
  [ (x1, y1) | (dx, dy) <- ds, let x1 = x + dx, let y1 = y + dy
  , x1 >= 0 || y1 > 0, y1 >= 0, not $ elem (x1, y1) banned
  ]

f 0 pts _        = [pts]
f n pts (as, bs) = let
  recurse p = let (xs, ys) = break (== p) bs in
    f (n - 1) (p:pts) (as++xs, (adj p (as++bs++pts)++tail ys))
  in concatMap recurse bs

ominos n = canon <$> f (n-1) [(0, 0)] ([], [(0, 1), (1, 0)])

rotate  p = sort $ (\(x, y) -> (maximum (snd <$> p) - y, x)) <$> p
reflect p = sort $ (\(x, y) -> (maximum (fst <$> p) - x, y)) <$> p

canTrans p q = elem p $
  take 4 (iterate rotate q) ++ take 4 (iterate rotate $ reflect q)

(boardx, boardy) = (10, 6)
lim = boardx * boardy

toNums (k, p) = let
  syms = nub $ take 4 (iterate rotate p) ++ take 4 (iterate rotate $ reflect p)
  in (lim + k :) <$> [(\(x, y) -> x + dx + boardx*(y + dy)) <$> p |
    p <- syms,
    let (xmax, ymax) = (maximum (fst <$> p), maximum (snd <$> p)),
    dx <- [0..boardx - 1 - xmax], dy <- [0..boardy - 1 - ymax]]

main = let
  sets = concatMap toNums $ zip [0..] (nubBy canTrans $ ominos 5)
  in putStrLn $ show $ length $ dlxSolve sets []

-- Prints an equivalent program that uses the C version of my DLX library.
mainC = let
  sets = concatMap toNums $ zip [0..] (nubBy canTrans $ ominos 5)
  in do
    putStr $ unlines $
      [ "#include <stdio.h>"
      , "#include \"dlx.h\""
      , "int main() { dlx_t dlx = dlx_new();"
      ] ++ (concatMap
      (\(a, as) -> (\b -> "dlx_set(dlx, "++show a++", "++show b++");") <$> as)
      $ zip [0..] sets) ++
      [ "int n = 0; void f() { n++; }"
      , "dlx_forall_cover(dlx, f);"
      , "printf(\"%d\\n\", n);"
      , "dlx_clear(dlx);"
      , "return 0;"
      , "}"
      ]
