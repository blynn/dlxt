import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List

data Cell = Cell { up, down, left, right, num, col :: Int } deriving (Eq, Show)

mkCell n = Cell n n n n 0 0

-- e.g. dlxSolve [ [0, 3]
--               , [1, 3]
--               , [1, 2]
--               , [0]]
dlxSolve asgns = let
  rows = length asgns
  cols = 1 + (foldl1' max $ concat asgns)
  ct = sum $ length <$> asgns
  in runST $ do
    tmp <- newListArray (0, cols + rows + ct) $ map mkCell [0..] :: ST s (STArray s Int Cell)
    nref <- newSTRef $ cols + rows + 1
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
            writeArray tmp n cell { col = cn }
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

-- TODO UPDATE S
      coverCol c = do
        cell <- readArray tmp c
        connectLR (left cell) (right cell)
        cs <- go c down
        forM_ cs $ \c -> do
           cs <- go c right
           forM_ cs $ \c -> do
             cell <- readArray tmp c
             connectUD (up cell) (down cell)

      uncoverCol c = do
        cell <- readArray tmp c
        cs <- go c down
        forM_ cs $ \c -> do
           cs <- go c right
           forM_ cs $ \c -> do
             cell <- readArray tmp c
             connectUD (up cell) c
             connectUD c (down cell)
        connectLR (left cell) c
        connectLR c (right cell)

      solve f sol = do
        cs <- todo
        case cs of
          [] -> f sol
          cs -> let
            (c, s) = foldl1' (\a b -> if snd b < snd a then b else a) cs
            in if s == 0 then return () else do
              coverCol c
              cs <- go c down
              forM_ cs $ \c -> do
                cs <- go c right
                forM_ cs $ \c -> do
                  cell <- readArray tmp c
                  coverCol $ col cell
                solve f (c:sol)
                forM_ cs $ \c -> do
                  cell <- readArray tmp c
                  uncoverCol $ col cell
                
              uncoverCol c
              {-
              t <- getAssocs tmp
              trace (unlines $ show <$> t) $ return ()
              -}

    -- zipWithM connectUD ((cols+) <$> (rows:[1..rows-1])) ((cols+) <$> [1..])
    zipWithM connectLR (cols:[0..cols-1]) [0..]
    zipWithM addRow [cols+1..] asgns

    -- getAssocs tmp
    let f sol = traceShow ("SOL: ", sol) $ return ()

    solve f []
    todo

--main = putStr . unlines $ show <$> dlxSolve [[0, 3], [0]]
main = putStr . unlines $ show <$> dlxSolve [[0, 3], [1, 2, 3], [1, 2], [0]]
