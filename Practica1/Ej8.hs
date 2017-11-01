media :: Fractional p => [p] -> p
media [] = 0
media l = (sum l) / (fromIntegral (length l))