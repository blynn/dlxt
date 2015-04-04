{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
--module DLX (dlxSolve) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Data.List.Split
import Data.Char

-- e.g. dlxSolve [ [0,       3]
--               , [   1, 2, 3]
--               , [   1, 2   ]
--               , [0         ] ]
dlxSolve :: [[Int]] -> [Int] -> [[Int]]
dlxSolve rows picks = runST $ do
  let
    cols = 1 + maximum (concat rows)
    csum = scanl1 (+) (length <$> rows)

  -- Instead of a record with fields, we keep an array of 5 ints for each
  -- cell, which we flatten. Offsets:
  --   0 = up, 1 = down, 2 = left, 3 = right, 4 = num
  --
  -- For a given column, let 's' be the number of possible row choices.
  -- 0                 = root cell; LR-linked list of columns headers
  -- (5*)<$>[1..cols]  = column headers; num field holds 's'
  -- (5*)<$>[cols+1..] = input cells; num holds the column header
  tmp <- newArray (0, 5*(1 + cols + sum (length <$> rows)) - 1) $ 0
      :: ST s (STUArray s Int Int)
  let
    linkUD i j = $( [| writeArray tmp (i+1) j >> writeArray tmp j i |] )
    linkLR i j = $( [| writeArray tmp (i+3) j >> writeArray tmp (j+2) i |] )
    hideLR c = $( [| do
      -- Slower:
      --   join $ linkLR <$> readArray tmp (c + 2) <*> readArray tmp (c + 3)
      left <- readArray tmp $ c + 2
      right <- readArray tmp $ c + 3
      linkLR left right |] )
    hideUD c = $( [| do
      up <- readArray tmp c
      down <- readArray tmp $ c + 1
      linkUD up down |] )
    unhideLR c = $( [| do
      readArray tmp (c + 2) >>= flip (writeArray tmp) c . (+ 3)
      readArray tmp (c + 3) >>= flip (writeArray tmp) c . (+ 2) |] )
    unhideUD c = $( [| do
      readArray tmp c >>= flip (writeArray tmp) c . (+ 1)
      readArray tmp (c + 1) >>= flip (writeArray tmp) c |] )
    incS cn = $( [| readArray tmp (cn + 4)
      >>= writeArray tmp (cn + 4) . (+1) |] )
    decS cn = $( [| readArray tmp (cn + 4)
      >>= writeArray tmp (cn + 4) . (+(-1)) |] )
    numOf = $( [| \c -> readArray tmp $ c + 4 |] )

    addRow (r, cs) = let 
      n = length cs
      f (k, c) = let cn = 5*(c+1) in do
        up <- readArray tmp cn
        linkUD up k
        linkUD k cn
        writeArray tmp (k + 4) cn
        incS cn
      g i = 5*(i+r)
      in mapM_ f (zip (g<$>[1..]) cs)
          >> zipWithM_ linkLR (g<$>(n:[1..])) (g<$>[1..n])

  forM_ ((5*)<$>(cols:[1..cols]))
    $ \c -> writeArray tmp c c >> writeArray tmp (c + 1) c
  zipWithM_ linkLR ((5*)<$>(cols:[0..])) ((5*)<$>[0..cols])
  mapM_ addRow $ zip ((cols +) <$> 0:csum) rows

  let
    go dir c0 = readArray tmp (c0 + dir) >>= f
      where f c | c == c0 = return []
                | True    = (c:) <$> (readArray tmp (c + dir) >>= f)

    coverCol col =
      hideLR col >>
      go 1 col >>= mapM_ (go 3 >=> mapM_ (\c -> numOf c >>= decS >> hideUD c))

    uncoverCol col =
      go 0 col >>= mapM_ (go 2 >=> mapM_ (\c -> numOf c >>= incS >> unhideUD c))
      >> unhideLR col

    findRow n = snd . head
      $ dropWhile ((<= div n 5 - cols - 1) . fst) $ zip csum [0..]

    todo = go 3 0 >>= \cs -> zip cs <$> mapM numOf cs
    -- todo = go 3 0 >>= (<$>) <$> zip <*> mapM numOf

    solve sol = todo >>= \css -> case css of
      [] -> return $ [findRow <$> sol]
      _  -> case foldl1' (\a b -> if snd b < snd a then b else a) css of
        (_  , 0) -> return []
        (col, _) -> do
          coverCol col
          sols <- go 1 col >>= mapM (\c -> do
            go 3 c >>= mapM_ ((coverCol =<<) . numOf)
            sols <- solve (c:sol)
            go 2 c >>= mapM_ ((uncoverCol =<<) . numOf)
            return sols)
          uncoverCol col
          return $ concat sols

  forM_ picks (\r -> let c = (0:csum)!!r in
    if c == csum!!r then return ()  -- Ignore empty rows.
      else let n = 5*(c + cols + 1) in
        numOf n >>= coverCol >> go 3 n >>= mapM_ ((coverCol =<<) . numOf))
       
  solve []

{-
main0 = print $ dlxSolve [ [0, 3] , [1, 2, 3] , [1, 2] , [0]] []

main1 = let
  -- Example from http://en.wikipedia.org/wiki/Knuth's_Algorithm_X
  ex = ((+(-1)) <$>) <$>
    [ [1, 4, 7]
    , [1, 4]
    , [4, 5, 7]
    , [3, 5, 6]
    , [2, 3, 6, 7]
    , [2, 7]
    ]
  in putStr . unlines $ show <$> dlxSolve ex []
-}

main :: IO ()
main = let
  ex = concat
    [ ".......1."
    , "4........"
    , ".2......."
    , "....5.4.7"
    , "..8...3.."
    , "..1.9...."
    , "3..4..2.."
    , ".5.1....."
    , "...8.6..."
    ]
  b9 a b c = 81*a + 9*b + c
  con r c d = [b9 0 r c, b9 1 r d, b9 2 c d, b9 3 ((div r 3)*3+(div c 3)) d]
  board = (\(ch, n) -> 9*n + digitToInt ch - 1)
    <$> filter ((/='.') . fst) (zip ex [0..])
  ds = [0..8]
  in putStr . unlines . chunksOf 9 . (intToDigit . (+1) . (`mod` 9) <$>)
    . sort $ board ++ (head $ dlxSolve (con <$> ds <*> ds <*> ds) board)
