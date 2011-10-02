import  Data.Char


encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg


