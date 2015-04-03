import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List
import Data.List.Split
import Data.Char

-- e.g. dlxSolve [ [0, 3]
--               , [1, 2, 3]
--               , [1, 2]
--               , [0]]
dlxSolve rows picks = runST $ do
  let
    cols = 1 + maximum (concat rows)
    csum = scanl1 (+) (length <$> rows)
    ct = sum $ length <$> rows
  -- 0 = up, 1 = down, 2 = left, 3 = right, 4 = num
  tmp <- newArray (0, 5*(1 + cols + ct) - 1) $ 0 :: ST s (STUArray s Int Int)
  let
    connectUD i j = writeArray tmp (i+1) j >> writeArray tmp j i
    connectLR i j = writeArray tmp (i+3) j >> writeArray tmp (j+2) i

    addRow r cs = let 
      n = length cs
      f k c = let cn = 5*(c+1) in do
        up <- readArray tmp cn
        connectUD up (g k)
        connectUD (g k) cn
        writeArray tmp (g k + 4) cn
        readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (1+)
        return (k + 1)

      g k = 5*(k+r)
      in do
        foldM f 1 cs
        zipWithM connectLR (g<$>(n:[1..])) (g<$>[1..n])
        return $ r + n
{-
    todo = let
      f 0 = return []
      f c = do
        s <- readArray tmp (c + 4)
        ((c, s):) <$> (f <=< readArray tmp $ c + 3)
      in readArray tmp 3 >>= f
-}

    todo = do
      cs <- go 3 0
      zip cs <$> mapM (readArray tmp . (+ 4)) cs

    go dir c0 = let
      f c
        | c == c0 = return []
        | True    = (c:) <$> (readArray tmp (c + dir) >>= f)
      in readArray tmp (c0 + dir) >>= f

    coverCol c = do
      left <- readArray tmp (c + 2)
      right <- readArray tmp (c + 3)
      connectLR left right
      -- Slower:
      -- join $ connectLR <$> readArray tmp (c + 2) <*> readArray tmp (c + 3)
      go 1 c >>= mapM_ (go 3 >=> mapM_ (\c -> do
        cn <- readArray tmp (c + 4)
        readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+ (-1))
        up <- readArray tmp c
        down <- readArray tmp $ c + 1
        connectUD up down))

    uncoverCol c = do
      go 0 c >>= mapM_ (go 2 >=> mapM_ (\c -> do
        cn <- readArray tmp (c + 4)
        readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+ 1)
        readArray tmp c >>= flip (writeArray tmp) c . (+ 1)
        readArray tmp (c + 1) >>= flip (writeArray tmp) c))
      readArray tmp (c + 2) >>= flip (writeArray tmp) c . (+ 3)
      readArray tmp (c + 3) >>= flip (writeArray tmp) c . (+ 2)

  forM_ ((5*)<$>(cols:[1..cols]))
    $ \c -> writeArray tmp c c >> writeArray tmp (c+1) c
  zipWithM connectLR ((5*)<$>(cols:[0..])) ((5*)<$>[0..cols])
  foldM addRow cols rows

  let
    findRow n = snd . head
      $ dropWhile ((<=(div n 5) - cols - 1) . fst) $ zip csum [0..]

    solve sol = do
      cs <- todo
      case cs of
        [] -> return $ [findRow <$> sol]
        _  -> let
          (c, s) = foldl1' (\a b -> if snd b < snd a then b else a) cs
          in if s == 0 then return [] else do
            coverCol c
            sols <- go 1 c >>= mapM (\c -> do
              go 3 c >>= mapM_ (\c -> readArray tmp (c + 4) >>= coverCol)
              sols <- solve (c:sol)
              go 2 c >>= mapM_ (\c -> readArray tmp (c + 4) >>= uncoverCol)
              return sols)
            uncoverCol c
            return $ concat sols

  forM_ picks (\r -> let c = (0:csum)!!r in
    -- Ignore empty rows.
    if c == csum!!r then return () else let n = 5*(c + cols + 1) in do
      readArray tmp (n + 4) >>= coverCol
      go 3 n >>= mapM_ (\c -> readArray tmp (c + 4) >>= coverCol))
       
  solve []

-- Example from http://en.wikipedia.org/wiki/Knuth's_Algorithm_X
wpex = ((+(-1)) <$>) <$>
  [ [1, 4, 7]
  , [1, 4]
  , [4, 5, 7]
  , [3, 5, 6]
  , [2, 3, 6, 7]
  , [2, 7]
  ]

wpmain = putStr . unlines $ show <$> dlxSolve wpex []
main0 = print $ dlxSolve [ [0, 3] , [1, 2, 3] , [1, 2] , [0]] []

sudex = concat
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

g ch n = 9*n + digitToInt ch - 1

board = uncurry g <$> filter ((/='.') . fst) (zip sudex [0..])

ds = [0..8]

main = putStr . unlines . ((intToDigit <$>)<$>) . chunksOf 9 . ((+1) . (`mod`9) <$>) . sort $ board ++ (head $ dlxSolve (con <$> ds <*> ds <*> ds) board)
