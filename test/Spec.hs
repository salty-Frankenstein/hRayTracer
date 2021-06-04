import Random

main :: IO ()
main = do--putStrLn "Test suite not yet implemented"
  r <- randFast 10
  
  print r
