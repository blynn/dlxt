import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List
import Data.List.Split
import Data.Char

data Cell = Cell { up, down, left, right, num :: Int } deriving (Eq, Show)

mkCell n = Cell n n n n 0

-- e.g. dlxSolve [ [0, 3]
--               , [1, 2, 3]
--               , [1, 2]
--               , [0]]
dlxSolve rows picks = runST $ do
  let
    cols = 1 + maximum (concat rows)
    csum = scanl1 (+) (length <$> rows)
    ct = sum $ length <$> rows
  tmp <- newListArray (0, cols + ct) $ map mkCell [0..] :: ST s (STArray s Int Cell)
  nref <- newSTRef $ cols + 1
  let
    connectUD i j = do
      cell <- readArray tmp i
      writeArray tmp i cell { down = j }
      cell <- readArray tmp j
      writeArray tmp j cell { up = i }

    connectLR i j = do
      cell <- readArray tmp i
      writeArray tmp i cell { right = j }
      cell <- readArray tmp j
      writeArray tmp j cell { left = i }

    addRow r cs = let 
      helper start prev [] = connectLR prev start

      helper start prev (c:cs) = do
        -- "malloc"
        n <- readSTRef nref
        writeSTRef nref $ n + 1

        -- Insert into column list.
        let cn = c + 1 in do
          ccell <- readArray tmp cn
          connectUD (up ccell) n
          connectUD n cn
          -- Set col number.
          cell <- readArray tmp n
          writeArray tmp n cell { num = cn }
          -- Increment s-count of column.
          cell <- readArray tmp cn
          writeArray tmp cn cell { num = 1 + num cell }

        -- Insert into row list.
        connectLR prev n

        helper start n cs

      in do
        n <- readSTRef nref
        helper n n cs

    todo = let
      f 0 = return []
      f c = do
        cell <- readArray tmp c
        rest <- f $ right cell
        return ((c, num cell):rest)
      in readArray tmp 0 >>= f . right

    go c0 dir = let
      f c
        | c == c0 = return []
        | True    = do
          cell <- readArray tmp c
          rest <- f $ dir cell
          return (c:rest)
      in readArray tmp c0 >>= f . dir

    coverCol c = do
      cell <- readArray tmp c
      connectLR (left cell) (right cell)
      cs <- go c down
      forM_ cs $ \c -> do
         cs <- go c right
         forM_ cs $ \c -> do
           cell <- readArray tmp c
           ccell <- readArray tmp $ num cell
           writeArray tmp (num cell) ccell { num = num ccell - 1 }
           connectUD (up cell) (down cell)

    uncoverCol c = do
      cell <- readArray tmp c
      cs <- go c up
      forM_ cs $ \c -> do
         cs <- go c left
         forM_ cs $ \c -> do
           cell <- readArray tmp c
           ccell <- readArray tmp $ num cell
           writeArray tmp (num cell) ccell { num = num ccell + 1 }
           connectUD (up cell) c
           connectUD c (down cell)
      connectLR (left cell) c
      connectLR c (right cell)

  zipWithM connectLR (cols:[0..cols-1]) [0..]
  zipWithM addRow [cols+1..] rows

  let
    findRow n = snd . head $ dropWhile ((<=n-cols-1) . fst) $ zip csum [0..]

    solve sol = do
      cs <- todo
      case cs of
        [] -> return $ [findRow <$> sol]
        cs -> let
          (c, s) = foldl1' (\a b -> if snd b < snd a then b else a) cs
          in if s == 0 then return [] else do
            coverCol c
            cs <- go c down
            sols <- forM cs $ \c -> do
              cs <- go c right
              forM_ cs $ \c -> readArray tmp c >>= coverCol . num
              sols <- solve (c:sol)
              cs <- go c left
              forM_ cs $ \c -> readArray tmp c >>= uncoverCol . num
              return sols
            uncoverCol c
            return $ concat sols
  {-
  t <- getAssocs tmp
  trace (unlines $ show <$> t) $ return ()
  -}

  mapM_ (\r -> let c = (0:csum)!!r in
    -- Ignore empty rows.
    if c == csum!!r then return () else let n = c + cols + 1 in do
      readArray tmp n >>= coverCol . num
      cs <- go n right
      forM_ cs $ \c -> readArray tmp c >>= coverCol . num) picks
       
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

wpmain = putStr . unlines $ show <$> dlxSolve wpex [3]

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
