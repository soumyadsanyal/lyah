module Traffic (...) where


data PrimaryColors = Red | Green | Blue

instance Eq PrimaryColors where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False

instance Show PrimaryColors where
    show Red = "Red is hot!"
    show Green = "Green is soft!"
    show Blue = "Blue is cool!"




