main :: IO ()
main = do
    let listOfTuples = [ ([True, False, True], [3.14, 2.71, 0.0])
                       , ([False, False], [1.23, 4.56, -7.89])
                       , ([True], [0.999, -1.0, 2.5])
                       ]
    print listOfTuples
