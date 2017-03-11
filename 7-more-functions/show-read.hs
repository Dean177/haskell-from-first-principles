module ShowRead where
  roundTrip :: (Show a, Read a) => a -> a
  roundTrip = read . show

  roundTrip2 :: (Show a, Read b) => a -> b
  roundTrip2 x = read (show x)

  main = do
    print (roundTrip 4)

    print ((roundTrip2 :: Int -> Int) 4)
