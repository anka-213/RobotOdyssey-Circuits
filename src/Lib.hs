module Lib
    -- ( someFunc
    -- )
where

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  hello
  world
  let _x = 5 :: Int
      _y = 3 :: Int
  return ()

hello :: IO ()
hello = undefined

world :: IO ()
world = undefined
