import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Data.List.Split
import Data.Char

-- e.g. dlxSolve [ [0, 3]
--               , [1, 2, 3]
--               , [1, 2]
--               , [0]]
dlxSolve :: [[Int]] -> [Int] -> [[Int]]
dlxSolve rows picks = runST $ do
  let
    cols = 1 + maximum (concat rows)
    csum = scanl1 (+) (length <$> rows)
    ct = sum $ length <$> rows
  -- 0 = up, 1 = down, 2 = left, 3 = right, 4 = num
  tmp <- newArray (0, 5*(1 + cols + ct) - 1) $ 0 :: ST s (STUArray s Int Int)
  let
    linkUD i j = writeArray tmp (i+1) j >> writeArray tmp j i
    linkLR i j = writeArray tmp (i+3) j >> writeArray tmp (j+2) i

    addRow r cs = let 
      n = length cs
      f k c = let cn = 5*(c+1) in do
        up <- readArray tmp cn
        linkUD up (g k)
        linkUD (g k) cn
        writeArray tmp (g k + 4) cn
        readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+1)
        return (k + 1)
      g i = 5*(i+r)
      in do
        foldM_ f 1 cs
        zipWithM_ linkLR (g<$>(n:[1..])) (g<$>[1..n])
        return $ r + n

  forM_ ((5*)<$>(cols:[1..cols]))
    $ \c -> writeArray tmp c c >> writeArray tmp (c+1) c
  zipWithM_ linkLR ((5*)<$>(cols:[0..])) ((5*)<$>[0..cols])
  foldM_ addRow cols rows

  let
    todo = go 3 0 >>= \cs -> zip cs <$> mapM (readArray tmp . (+ 4)) cs

    go dir c0 = let
      f c
        | c == c0 = return []
        | True    = (c:) <$> (readArray tmp (c + dir) >>= f)
      in readArray tmp (c0 + dir) >>= f

    coverCol col = do
      -- Slower:
      --   linkLR left right
      -- Even slower:
      --   join $ linkLR <$> readArray tmp (col + 2) <*> readArray tmp (col + 3)
      left <- readArray tmp (col + 2)
      right <- readArray tmp (col + 3)
      writeArray tmp (left+3) right >> writeArray tmp (right+2) left
      go 1 col >>= mapM_ (go 3 >=> mapM_ (\c -> do
        cn <- readArray tmp (c + 4)
        readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+ (-1))
        up <- readArray tmp c
        down <- readArray tmp $ c + 1
        -- Slower: linkUD up down
        writeArray tmp (up+1) down >> writeArray tmp down up))

    uncoverCol col = do
      go 0 col >>= mapM_ (go 2 >=> mapM_ (\c -> do
        cn <- readArray tmp (c + 4)
        readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+ 1)
        readArray tmp c >>= flip (writeArray tmp) c . (+ 1)
        readArray tmp (c + 1) >>= flip (writeArray tmp) c))
      readArray tmp (col + 2) >>= flip (writeArray tmp) col . (+ 3)
      readArray tmp (col + 3) >>= flip (writeArray tmp) col . (+ 2)

  let
    findRow n = snd . head
      $ dropWhile ((<=(div n 5) - cols - 1) . fst) $ zip csum [0..]

    solve sol = do
      cs <- todo
      case cs of
        [] -> return $ [findRow <$> sol]
        _  -> let
          (col, s) = foldl1' (\a b -> if snd b < snd a then b else a) cs
          in if s == 0 then return [] else do
            coverCol col
            sols <- go 1 col >>= mapM (\col' -> do
              go 3 col' >>= mapM_ (\c -> readArray tmp (c + 4) >>= coverCol)
              sols <- solve (col':sol)
              go 2 col' >>= mapM_ (\c -> readArray tmp (c + 4) >>= uncoverCol)
              return sols)
            uncoverCol col
            return $ concat sols

  forM_ picks (\r -> let c = (0:csum)!!r in
    if c == csum!!r then return ()  -- Ignore empty rows.
      else let n = 5*(c + cols + 1) in do
        readArray tmp (n + 4) >>= coverCol
        go 3 n >>= mapM_ ((coverCol =<<) . readArray tmp . (+ 4)))
       
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
  in  putStr . unlines $ show <$> dlxSolve ex []
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
  in putStr . unlines . ((intToDigit <$>)<$>) . chunksOf 9 . ((+1) . (`mod`9)
    <$>) . sort $ board ++ (head $ dlxSolve (con <$> ds <*> ds <*> ds) board)
