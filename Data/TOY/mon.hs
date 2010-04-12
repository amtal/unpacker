{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Maybe

state :: State (Int,Int) Int
state = do
    l <- gets fst
    r <- gets snd
    put $ (l+1,r-1)
    return $ (l+r)*2

mayb :: Int -> Int -> Maybe Int
mayb a b | b==0      = Nothing
         | otherwise = Just $ a`div`b

wrt :: Writer [Int] Int
wrt = do
    tell [1]
    (_,ns) <- listen $ do
        tell [2]
        tell [3]
        tell [4]
        return 99
    tell ns
    tell [5]
    pass $ return (1,fmap (+7))

test :: MaybeT (StateT Int (Writer String)) Int
test = do
    lift . lift $ tell "start\n"
    n <- lift get
    lift $ put $ n-9
    lift . lift $ tell "end\n"
    return $ n+5

runTest = runWriter . flip runStateT 3 . runMaybeT
