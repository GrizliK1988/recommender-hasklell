module Rating.Rating
    (
    ratingsFromRow
    , ratingsWithGenderFromRow
    , ratingToFloat
    , popularity
    , existingRatings
    , ratingsGreaterThan
    , relativePopularity
    , correlation
    , avg
    , covariation
    ) where

ratingsFromRow :: [[Char]] -> [[Char]]
ratingsFromRow (_:(_:xs)) = xs

ratingsWithGenderFromRow :: [[Char]] -> [[Char]]
ratingsWithGenderFromRow (_:xs) = xs

popularity :: (Eq a, Fractional a) => (a, a) -> a
popularity (_, 0) = 0
popularity (x, y) = x / y

ratingToFloat :: [Char] -> Float
ratingToFloat "" = 0.0
ratingToFloat x = read x :: Float

existingRatings :: (Num a, Ord a) => [a] -> [a]
existingRatings ratings = [x | x <- ratings, x > 0]

ratingsGreaterThan :: (Num a, Ord a) => a -> [a] -> [a]
ratingsGreaterThan y ratings = [x | x <- ratings, x >= y]

relativePopularity :: (Fractional a, Ord a) => [a] -> [a] -> a
relativePopularity r1 r2 =
    let r1r2Real = length [
                        (x, y) |
                        (x, y) <- zip r1 r2,
                        x /= 0,
                        y /= 0]
        r2Real = length $ existingRatings r2
    in (fromIntegral r1r2Real) / (fromIntegral r2Real)

correlation :: (Ord a, Floating a, Fractional a) => [a] -> [a] -> a
correlation x y =
  let xy = [(xs, ys) | (xs, ys) <- zip x y, xs > 0, ys > 0]
      header = covariation xy
      footer = (deviation $ map fst xy) * (deviation $ map snd xy)
  in header / footer

covariation :: (Ord a, Fractional a) => [(a, a)] -> a
covariation xy =
  let avgX = avg $ map fst xy
      avgY = avg $ map snd xy
  in sum [(xs - avgX)*(ys - avgY) | (xs, ys) <- xy]

deviation :: (Ord a, Fractional a, Floating a) => [a] -> a
deviation x =
  let avgX = avg x
  in sqrt $ sum [(xs - avgX)^2 | xs <- x]

avg :: (Ord a, Fractional a) => [a] -> a
avg x = (sum x) / (fromIntegral $ length $ existingRatings x)
