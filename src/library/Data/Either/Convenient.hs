module Data.Either.Convenient (
                                eitherToMaybe
                              ) where


eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
