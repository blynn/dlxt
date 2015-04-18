import System.Process

main :: IO ()
main = do
  -- From http://school.maths.uwa.edu.au/~gordon/sudokumin.php
  s <- readProcess "./suds" [] $ unlines
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
  if s /= unlines
    [ "693784512"
    , "487512936"
    , "125963874"
    , "932651487"
    , "568247391"
    , "741398625"
    , "319475268"
    , "856129743"
    , "274836159"
    ] then putStrLn "FAILED" else return ()
