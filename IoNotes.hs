{--
    Combinators: actions on IO operations
    (>>=) :: IO a -> (a -> IO b) -> IO b
    Passes the result of the left IO op. as an arg into the function on the right
    getLine >>= someFunc 

    (*>) :: IO a -> IO b -> IO b
    Sequence two IO ops, discarding payload of first

    pure :: a -> IO a
    "lift" a value into IO context, does not add any I/O effects

    fmap :: (a -> b) -> IO a -> IO b
    Apply a fn to the payload value of an IO op

    IO is first class -- it can be passed to fns, put in containers, etc
    ex.
    whenIO :: IO Bool -> IO () -> IO ()
    whenIO cond action = 
        cond >>= \result ->
            if result
                then action
                else pure ()
--}