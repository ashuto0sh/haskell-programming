getNthItem :: [a] -> Int -> a
getNthItem [] _ = error "Empty list"
getNthItem (x:_) 1 = x
getNthItem (_:xs) n = getNthItem xs (n-1)

thirdLetter :: String -> Char
thirdLetter xs =  getNthItem xs 3

letterAtIndex :: Int -> Char
letterAtIndex n = getNthItem myStr n
    where myStr = "Curry Is Awesome"