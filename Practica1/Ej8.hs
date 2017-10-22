media :: (Foldable t, Fractional a) => t a -> a
media l
    | null l = 0
    | otherwise = (sum l) / (fromIntegral (length l))