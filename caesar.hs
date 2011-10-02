import  Data.Char


encode :: Int -> String -> String
encode offset msg = map (chr . (+ offset) . ord) msg


