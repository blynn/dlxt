import Debug.Trace
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
  tmp <- newArray (0, 5*(1 + cols + ct)) $ 0 :: ST s (STUArray s Int Int)
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

    todo = let
      f 0 = return []
      f c = do
        right <- readArray tmp (c + 3)
        s <- readArray tmp (c + 4)
        rest <- f right
        return ((c, s):rest)
      in readArray tmp 3 >>= f

    go c0 dir = let
      f c
        | c == c0 = return []
        | True    = do
          next <- readArray tmp (c + dir)
          rest <- f next
          return (c:rest)
      in readArray tmp (c0 + dir) >>= f

    coverCol c = do
      left <- readArray tmp (c + 2)
      right <- readArray tmp (c + 3)
      connectLR left right
      cs <- go c 1
      forM_ cs $ \c -> do
         cs <- go c 3
         forM_ cs $ \c -> do
           cn <- readArray tmp (c + 4)
           readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+ (-1))
           up <- readArray tmp c
           down <- readArray tmp $ c + 1
           connectUD up down

    uncoverCol c = do
      cs <- go c 0
      forM_ cs $ \c -> do
         cs <- go c 2
         forM_ cs $ \c -> do
           cn <- readArray tmp (c + 4)
           readArray tmp (cn + 4) >>= writeArray tmp (cn + 4) . (+ 1)
           up <- readArray tmp c
           down <- readArray tmp $ c + 1
           writeArray tmp (up + 1) c
           writeArray tmp down c
      left <- readArray tmp (c + 2)
      right <- readArray tmp (c + 3)
      writeArray tmp (left + 3) c
      writeArray tmp (right + 2) c

  forM ((5*)<$>(cols:[1..cols])) (\c -> do writeArray tmp c c >> writeArray tmp (c+1) c)
  zipWithM connectLR ((5*)<$>(cols:[0..])) ((5*)<$>[0..cols])
  foldM addRow cols rows

  let
    findRow n = snd . head $ dropWhile ((<=(div n 5)-cols-1) . fst) $ zip csum [0..]

    solve sol = do
      cs <- todo
      case cs of
        [] -> return $ [findRow <$> sol]
        cs -> let
          (c, s) = foldl1' (\a b -> if snd b < snd a then b else a) cs
          in if s == 0 then return [] else do
            coverCol c
            cs <- go c 1
            sols <- forM cs $ \c -> do
              cs <- go c 3
              forM_ cs $ \c -> readArray tmp (c + 4) >>= coverCol
              sols <- solve (c:sol)
              cs <- go c 2
              forM_ cs $ \c -> readArray tmp (c + 4) >>= uncoverCol
              return sols
            uncoverCol c
            return $ concat sols
  {-
  t <- getAssocs tmp
  trace (unlines $ show <$> t) $ return ()
  -}

  mapM_ (\r -> let c = (0:csum)!!r in
    -- Ignore empty rows.
    if c == csum!!r then return () else let n = 5*(c + cols + 1) in do
      readArray tmp (n + 4) >>= coverCol
      cs <- go n 3
      forM_ cs $ \c -> readArray tmp (c + 4) >>= coverCol) picks
       
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

ds = [0..8]

nine a b c = 81*a + 9*b + c

f r c d = [nine 0 r c, nine 1 r d, nine 2 c d, nine 3 ((div r 3)*3+(div c 3)) d]

--main = putStr . unlines $ show <$> dlxSolve (f <$> ds <*> ds <*> ds) board

g ch n = (digitToInt ch - 1)+n*9

board = uncurry g <$> filter ((/='.') . fst) (zip sudex $ [0..])

main = putStr . unlines . ((intToDigit <$>)<$>) . chunksOf 9 . ((+1) . (`mod`9) <$>) . sort $ board ++ (head $ dlxSolve (f <$> ds <*> ds <*> ds) board)
