module Helper.Movie
    (
      Movie(..)
      , movies
    ) where

import Data.List.Split

data Movie = Movie { movieId :: Int
                     , title :: String
                     , genres :: [String] } deriving (Show)

movies :: [[String]] -> [Movie]
movies content =
  let (_:moviesContent) = content
  in map (\x -> Movie { movieId = read (x !! 0) :: Int
                      , title = x !! 1
                      , genres = splitOn "|" $ read (x !! 2) })
                      moviesContent
