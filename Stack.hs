module Stack where


data Stack a = EStack | Push a (Stack a)
    deriving (Show, Eq, Ord, Read)


