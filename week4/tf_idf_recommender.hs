import System.IO
import Data.CSV
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Helper.Tag
import Helper.Movie
import Helper.Rating
import Helper.Profile

main = do
  tagsCsv <- parseFromFile csvFile "/home/data/tags.csv"
  moviesCsv <- parseFromFile csvFile "/home/data/movies.csv"
  ratingsCsv <- parseFromFile csvFile "/home/data/ratings.csv"

  process tagsCsv moviesCsv ratingsCsv

process :: Either a [[String]] -> Either a [[String]] -> Either a [[String]] -> IO ()
process (Left _) _ _ = putStrLn "Error occurred"
process _ (Left _) _ = putStrLn "Error occurred"
process _ _ (Left _) = putStrLn "Error occurred"
process (Right tagsContent) (Right moviesContent) (Right ratingsContent) = do
  let tagsData = tags tagsContent
  let moviesData = movies moviesContent
  let ratingsData = ratings ratingsContent

  -- -- TF = количество документов, в которых встречается тег
  let tagsTF = tf tagsData
  let tagsQ = q moviesData tagsData
  let userProfiles = profiles ratingsData tagsQ

  putStrLn $ show $ tagsTF Map.! "sports"
  putStrLn $ show $ Map.filterWithKey (\(mId, _) _ -> mId == 2231) tagsQ
  putStrLn $ show userProfiles
  -- putStrLn $ show $ Map.filterWithKey (\(uId, _) _ -> uId == 320) userProfiles
  --
  -- -- putStrLn $ show $ item_score 320 32 userProfiles tagsQ

  putStrLn "Finished"
