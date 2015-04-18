import Control.Applicative
import Data.List
import DLX

works (sets, initial) want = sort (sort <$> dlxSolve sets initial) == want

f x | uncurry works x = return ()
    | otherwise       = putStrLn "FAILED:" >> putStrLn (show x)

main = mapM_ f
  -- Simple example with two solutions.
  [ (([[0, 3] , [1, 2, 3] , [1, 2] , [0]], [])
    , [[0, 2], [1, 3]])
  -- Example from http://en.wikipedia.org/wiki/Knuth's_Algorithm_X
  , ((((+(-1)) <$>) <$>
      [ [1, 4, 7]
      , [1, 4]
      , [4, 5, 7]
      , [3, 5, 6]
      , [2, 3, 6, 7]
      , [2, 7]
      ], [])
    , [[1, 3, 5]])
  ]
