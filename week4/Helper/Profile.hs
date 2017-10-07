module Helper.Profile
    (
    profiles
    , item_score
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Helper.Rating as Rating
import qualified Helper.Tag as Tag

profiles :: (Floating a, Ord a) => [Rating.Rating] -> Map.Map (Int, String) a -> Map.Map (Int, String) a
profiles ratings q =
  let uniqUids = Set.fromList [ Rating.userId r | r <- ratings, Rating.rating r >= 3.5 ]
  --     likedMovies = Set.map (\uId -> (uId, Set.toList (Rating.userLikedMoviesId uId ratings))) uniqUids
  --
  --     pSet = Set.map (\(uId, mIds) -> ( let terms = movieTerms mIds q
  --                                           termsList = Map.toList terms
  --                                           termsWithoutMId = map (\(x, y) -> (snd x, y)) termsList
  --                                           termsTFIDF = Map.fromListWith (+) termsWithoutMId
  --                                           termsTFIDFList = Map.toList termsTFIDF
  --                                       in map (\(tag, x) -> ((uId, tag), x)) termsTFIDFList )) likedMovies
  --     pList = foldr (++) [] $ Set.toList pSet
  --     p = Map.fromList pList
  -- in p
  in Map.fromList [((length uniqUids, "11"), 0.5)]

movieTerms :: (Floating a) => [Int] -> Map.Map (Int, String) a -> Map.Map (Int, String) a
movieTerms movieIds q = Map.filterWithKey (\(mId, _) _ -> (any (\x -> x == mId) movieIds)) q

item_score :: (Eq a, Floating a) => Int -> Int -> Map.Map (Int, String) a -> Map.Map (Int, String) a -> a
item_score uId mId p q =
  let p_u = Map.mapKeys snd $ Map.filterWithKey (\(x, _) _ -> x == uId) p
      q_m = Map.mapKeys snd $ Map.filterWithKey (\(x, _) _ -> x == mId) q
      numerator = Map.foldr (+) 0.0 (Map.intersectionWith (*) p_u q_m)
      denominator = (Tag.item_norm q mId) * (Tag.item_norm p uId)
  in if (denominator == 0) then 0.0 else (numerator / denominator)
