module Helper.Tag
    (
    Tag(..)
    , tags
    , tf
    , tfidf
    , item_tf
    , item_norm
    , q
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Helper.Movie as Movie
import qualified Helper.Rating as Rating

data Tag = Tag { movieId :: Int
                 , userId :: Int
                 , tag :: String
                 , timestamp :: Int } deriving (Show)

tags :: [[String]] -> [Tag]
tags content =
  let (_:tagsContent) = content
  in map (\x -> Tag { movieId = read (x !! 0) :: Int
                      , userId = read (x !! 1) :: Int
                      , tag = x !! 2
                      , timestamp = read (x !! 3) :: Int })
                 tagsContent

tf :: (Floating a) => [Tag] -> Map.Map String a
tf tags =
  let tagsMovies = map (\x -> (tag x, Set.fromList [movieId x])) tags
      tagsAgg = Map.fromListWith Set.union tagsMovies
  in Map.map (\x -> fromIntegral $ Set.size x) tagsAgg

tfidf :: (Floating a) => (Map.Map String a) -> a -> (Map.Map String a)
tfidf tf d = Map.map (\x -> (log d - log x)) tf

item_tf :: (Floating a) => [Tag] -> Map.Map (Int, String) a
item_tf tags =
  let tagsItemsFreq = map (\x -> ((movieId x, tag x), 1.0)) tags
  in Map.fromListWith (\x y -> x + y) tagsItemsFreq

item_norm :: (Floating a) => Map.Map (Int, String) a -> Int -> a
item_norm termFreqMatrix movieId =
  let movieTermFreqs = Map.filterWithKey (\(x, _) _ -> x == movieId) termFreqMatrix
      squareSum = Map.foldr (\x acc -> acc + x^2) 0.0 movieTermFreqs
  in sqrt squareSum

q :: (Floating a, Ord a) => [Movie.Movie] -> [Tag] -> Map.Map (Int, String) a
q movies tags =
  let tagsIDF = tfidf (tf tags) (fromIntegral $ length movies)
      movieTFs = item_tf tags
      movieTFIDFs = Map.mapWithKey (\(mId, tag) freq -> freq * (tagsIDF Map.! tag)) movieTFs
      norm mId = let n = item_norm movieTFIDFs mId
                 in if (n > 0) then n else 1
      movieNormTFIDFs = Map.mapWithKey (\(mId, tag) freq -> freq / (norm mId)) movieTFIDFs
  in movieNormTFIDFs
