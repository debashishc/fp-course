class Equal x where
    isEqual :: x -> x -> Bool

instance Equal TwoInts where
    -- isEqual :: TwoInts -> TwoInts -> Bool
    isEqual (TI a1 b1) (TI a2 b2) = 
        (a1 == a2) && (b1 == b2)
-- need one unique instance per data type

instance Equal Bool where
    -- isEqual :: Bool -> Bool -> Bool
    isEqual True  True  = True
    isEqual False False = True
    isEqual True  False = False
    isEqual False True  = True

data TwoInts 
    = TI Int Int
    deriving Show

isInList :: Equal a => a -> [a] -> Bool
isInList _ []       = False
isInList e (h : t)  = (e `isEqual` h) || isInList e t