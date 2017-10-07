module Helper.Rating
    (
      Rating(..)
      , ratings
      , userLikedMoviesId
    ) where

import Data.List.Split
import qualified Data.Set as Set

data Rating = Rating { userId :: Int
                       , movieId :: Int
                       , rating :: Float
                       , timestamp :: Int } deriving (Show)

ratings :: [[String]] -> [Rating]
ratings content =
  let (_:ratingsContent) = content
  in map (\x -> Rating { userId = read (x !! 0) :: Int
                        , movieId = read (x !! 1) :: Int
                        , rating = read (x !! 2) :: Float
                        , timestamp = read (x !! 3) :: Int })
                        ratingsContent

userLikedMoviesId :: Int -> [Rating] -> Set.Set Int
userLikedMoviesId uId ratings =
  let mIds = [ movieId r | r <- ratings, rating r >= 3.5, userId r == uId ]
  in Set.fromList mIds
