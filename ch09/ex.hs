module Exercise where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

pr1 = [(x, y) | x <- mySqr, y <- myCube]

pr2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
