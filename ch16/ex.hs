e :: IO Integer
e = 
    let
        ioi = readIO "12" :: IO Integer
        changed = fmap read (fmap ("123"++) (fmap show ioi)) :: IO Integer
    in fmap (*3) changed

main :: IO ()
main =
    do
        ll <- e
        putStrLn $ show ll