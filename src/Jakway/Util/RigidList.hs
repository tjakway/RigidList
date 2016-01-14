{-# LANGUAGE TypeSynonymInstances #-}
import Prelude hiding (Left, Right)
import Control.Monad
import Data.Maybe (fromJust)

data RigidList a = Left [Maybe a] | Right [Maybe a]

unwrapRigidList (Right a) = a
unwrapRigidList (Left a) = a

instance Monad RigidList where
        return = Right . return . return

        -- |This matches what you would expect
        -- because [] :: [Maybe a] is perfectly valid
        (>>=) (Left x) f = Left x
        (>>=) (Right y) f
                -- |Short circuit but don't change the list
                -- otherwise we might not know there was an error!
                | y `elem` Nothing = Left y
                | otherwise = map (f . fromJust) y
