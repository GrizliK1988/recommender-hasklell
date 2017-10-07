module Rating.Film
    (
    films
    , filmsRatings
    , filmsGenderRatings
    , filmRatingMeans
    ) where

import Data.List.Split
import Data.List
import Helper.StringHelper
import Rating.Rating

films :: [[Char]] -> [[Char]]
films table = (map (ratingsFromRow . splitOn "," . removeTrainlingR) table) !! 0

filmsRatings :: [[Char]] -> [[Float]]
filmsRatings rows =
  let (_:dataRows) = rows
      ratings = map ((map ratingToFloat) . ratingsFromRow . splitOn "," . removeTrainlingR) dataRows
  in transpose ratings

filmsGenderRatings :: Float -> [[Char]] -> [[Float]]
filmsGenderRatings gender rows =
  let (_:dataRows) = rows
      ratings = map ((map ratingToFloat) . ratingsWithGenderFromRow . splitOn "," . removeTrainlingR) dataRows
  in transpose [xs | (g:xs) <- ratings, g == gender]

filmRatingMeans :: (Fractional a, Ord a) => [[a]] -> [a]
filmRatingMeans ratings =
  let mean = \x -> (sum x)/(fromIntegral $ length $ existingRatings x)
  in map mean ratings
