module Helper.StringHelper
    (
    removeTrainlingR
    , join
    , join2
    , replace
    ) where

import Data.List.Split

removeTrainlingR :: [Char] -> [Char]
removeTrainlingR x = replace "\r" "" x

join :: Foldable t => [Char] -> t [Char] -> [Char]
join glue x = foldr (join2 glue) "" x

join2 :: [Char] -> [Char] -> [Char] -> [Char]
join2 glue "" y = y
join2 glue x "" = x
join2 glue x y = x ++ glue ++ y

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace old new = join new . splitOn old
