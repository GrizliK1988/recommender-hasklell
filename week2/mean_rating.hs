import System.IO
import Data.List
import Data.List.Split
import Data.Function
import Helper.StringHelper
import Rating.Rating
import Rating.Film

main = do
  handle <- openFile "HW1-data.csv" ReadMode

  matrixContent <- hGetContents handle

  let filmList = films $ lines matrixContent
  let ratings = filmsRatings $ lines matrixContent
  let maleRatings = filmsGenderRatings 0 (lines matrixContent)
  let femaleRatings = filmsGenderRatings 1 (lines matrixContent)

  putStrLn "Ordered by mean rating. Mean all, female, male"
  putStrLn ""

  let sndFrom4 (_, x, _, _) = x

  let filmsWithRatings = zip4 filmList
                              (filmRatingMeans ratings)
                              (filmRatingMeans maleRatings)
                              (filmRatingMeans femaleRatings)
  let filmsWithRatingsRows = map show (sortBy (flip compare `on` sndFrom4) filmsWithRatings)
  mapM_ putStrLn filmsWithRatingsRows


  let popularities = map (fromIntegral . length . existingRatings) ratings
  let malePopularities = map (fromIntegral . length . existingRatings) maleRatings
  let femalePopularities = map (fromIntegral . length . existingRatings) femaleRatings

  let filmsWithPopularitiesRows = showSortedTuples $ zip filmList popularities
  putStrLn ""
  putStrLn "=============================="
  putStrLn "Ordered by popularity"
  putStrLn ""
  mapM_ putStrLn filmsWithPopularitiesRows

  let likes = map (fromIntegral . length . (ratingsGreaterThan 4)) ratings
  let likesMale = map (fromIntegral . length . (ratingsGreaterThan 4)) maleRatings
  let likesFemale = map (fromIntegral . length . (ratingsGreaterThan 4)) femaleRatings

  let likesPopularities = map popularity $ zip likes popularities
  let maleLikesPopularities = map popularity $ zip likesMale malePopularities
  let femaleLikesPopularities = map popularity $ zip likesFemale femalePopularities

  let likesTuples = zip4 filmList
                         likesPopularities
                         maleLikesPopularities
                         femaleLikesPopularities
  let filmsWithLikesPopularities = map show (sortBy (flip compare `on` sndFrom4) likesTuples)

  putStrLn ""
  putStrLn "=============================="
  putStrLn "Ordered by percentage of likeness"
  putStrLn ""
  mapM_ putStrLn filmsWithLikesPopularities

  let relativePopularities = [ relativePopularity x (ratings !! 6) | x <- ratings]
  let relativePopularitiesRows = showSortedTuples $ zip filmList relativePopularities

  putStrLn ""
  putStrLn "=============================="
  putStrLn "Popularity to Toy Story"
  putStrLn ""
  mapM_ putStrLn relativePopularitiesRows

  let correlations = [ correlation x (ratings !! 6) | x <- ratings]
  let correlationRows = showSortedTuples $ zip filmList correlations

  putStrLn ""
  putStrLn "=============================="
  putStrLn "Correlations with Toy Story"
  putStrLn ""
  mapM_ putStrLn correlationRows

  hClose handle

showSortedTuples :: (Show a, Show b, Ord b) => [(a, b)] -> [[Char]]
showSortedTuples x = map show (sortBy (flip compare `on` snd) x)
