module Date where

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving (Show, Ord)

data Date = Date DayOfWeek Int
    deriving (Show, Ord)

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity a1) (Identity a2) =
        a1 == a2

instance Eq DayOfWeek where
    (==) Sun Sun = True
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) _ _     = False

instance Eq Date where
    (==) (Date weekDay dayOfMonth)
         (Date weekDay' dayOfMonth') =
             (weekDay == weekDay')
             && (dayOfMonth == dayOfMonth')