module TrafficLight (...) where


data TrafficLight = Red | Yellow | Green 

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red is stop!"
    show Yellow = "Yellow is lay off the gas!"
    show Green = "Green is go!"




