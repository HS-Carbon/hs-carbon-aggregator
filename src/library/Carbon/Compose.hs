module Carbon.Compose (
                        compose
                      ) where

compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v
