module Calendar
    ( Date
    , Year
    , Mounth
    , Day
    , Week
    , Time
    , Hour
    , Minute
    , Second
    )
where

data Date = Date Year Mounth Day

newtype Year = Year Int

data Mounth = January | February | March
            | April   | May      | June
            | July    | August   | September
            | October | November | December

instance Show Mounth where
    show January   = "Jan"
    show February  = "Feb"
    show March     = "Mar"
    show April     = "Apr"
    show May       = "May"
    show June      = "Jun"
    show July      = "Jul"
    show August    = "Aug"
    show September = "Sep"
    show October   = "Oct"
    show November  = "Nov"
    show December  = "Dec"

newtype Day = Day Int

data Week = Monday   | Tuesday | Wednesday
          | Thursday | Friday  | Saturday
          | Sunday

instance Show Week where
    show Monday    = "Mon"
    show Tuesday   = "Tue"
    show Wednesday = "Wed"
    show Thursday  = "Thu"
    show Friday    = "Fri"
    show Saturday  = "Sat"
    show Sunday    = "Sun"

data Time = Time Hour Minute Second

instance Show Time where
    show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

newtype Hour = Hour Int
newtype Minute = Minute Int
newtype Second = Second Int

instance Show Hour where
    show (Hour h) = addZero (show h)

instance Show Minute where
    show (Minute m) = addZero (show m)

instance Show Second where
    show (Second s) = addZero (show s)

addZero :: String -> String
addZero [a] = ['0', a]
addZero a   = a
