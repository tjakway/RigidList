{-# LANGUAGE TypeSynonymInstances #-}
import Control.Monad
import Data.Maybe (fromJust)

newtype RigidList a = RigidList [Maybe a]

instance Monad RigidList where
        return = return . return

        -- |This matches what you would expect
        -- because [] :: [Maybe a] is perfectly valid
        (>>=) [] _ = []
        (>>=) a f
                -- |Short circuit but don't change the list
                -- otherwise we might not know there was an error!
                | a `elem` Nothing = a
                | otherwise = a >>= (f . fromJust)
