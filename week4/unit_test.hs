import Test.HUnit
import qualified Data.Map as Map
import Helper.Tag
import qualified Helper.Movie as Movie
import qualified Helper.Profile as Profile
import qualified Helper.Rating as Rating

testTf = TestCase (
  do
    let empty = tf []
    assertEqual "Expected empty map" Map.empty empty

    let single = tf [ Tag {movieId = 101, userId = 1, tag = "tag_1", timestamp = 100} ]
    assertEqual "Expected map with 1 element" (Map.fromList [("tag_1", 1)]) single

    let simple = tf [ Tag {movieId = 101, userId = 1, tag = "tag_1", timestamp = 100}
                      , Tag {movieId = 101, userId = 1, tag = "tag_2", timestamp = 100} ]
    assertEqual "2 tags appears once in 2 movies" (Map.fromList [("tag_1", 1), ("tag_2", 1)]) simple

    let merge = tf [ Tag {movieId = 101, userId = 1, tag = "tag_1", timestamp = 100}
                     , Tag {movieId = 102, userId = 1, tag = "tag_1", timestamp = 100}
                     , Tag {movieId = 103, userId = 1, tag = "tag_2", timestamp = 100}
                     , Tag {movieId = 103, userId = 1, tag = "tag_1", timestamp = 100}
                     ]
    assertEqual "Tag 1 appears 3 times" (Map.fromList [("tag_1", 3), ("tag_2", 1)]) merge

    let duplicates = tf [ Tag {movieId = 101, userId = 1, tag = "tag_1", timestamp = 100}
                     , Tag {movieId = 101, userId = 2, tag = "tag_1", timestamp = 100}
                     ]
    assertEqual "Tag 1 appears 2 times, but in the same movie" (Map.fromList [("tag_1", 1)]) duplicates
    )

testTfidf = TestCase (
  do
    let single = tfidf (Map.fromList [("tag", 1)]) 1
    assertEqual "Expected IDF == 0.0 for single tag"
                (Map.fromList [("tag", 0.0)])
                single

    let single2 = tfidf (Map.fromList [("tag", exp 1)]) (exp 2)
    assertEqual "Expected IDF == 1.0 for single tag"
                (Map.fromList [("tag", 1.0)])
                single2
    )

testItemTf = TestCase (
  do
    let oneMovieOneTag = item_tf [ Tag {movieId = 101, tag = "tag", userId = 1, timestamp = 1001} ]

    assertEqual "Expected [(101, \"tag\"): 1]"
                (Map.fromList [((101, "tag"), 1)])
                oneMovieOneTag
    --

    let oneMovieTwoTags = item_tf [ Tag {movieId = 101, tag = "tag1", userId = 1, timestamp = 1001}
                                    , Tag {movieId = 101, tag = "tag2", userId = 1, timestamp = 1001} ]

    assertEqual "Expected [(101, \"tag1\"): 1, (101, \"tag2\"): 1]"
                (Map.fromList [((101, "tag1"), 1), ((101, "tag2"), 1)])
                oneMovieTwoTags
    --

    let oneMovieDoubleTag = item_tf [ Tag {movieId = 101, tag = "tag", userId = 1, timestamp = 1001}
                                    , Tag {movieId = 101, tag = "tag", userId = 2, timestamp = 1001} ]

    assertEqual "Expected [(101, \"tag\"): 2]"
                (Map.fromList [((101, "tag"), 2)])
                oneMovieDoubleTag
    --
    )

testItemNorm = TestCase (
  do
    let normForExistingMovie = item_norm (Map.fromList [((101, "tag1"), 3)
                                                        , ((101, "tag2"), 4)
                                                        , ((102, "tag1"), 4)]) 101

    assertEqual "Expected norm to be 5"
                5
                normForExistingMovie

    let normForMissingMovie = item_norm Map.empty 101

    assertEqual "Expected norm to be 0"
                0
                normForMissingMovie
    )


testQ = TestCase (
  do
    let empty = q [] []
    assertEqual "Expected empty map" Map.empty empty

    let singleTerm = q [ Movie.Movie {Movie.movieId = 101, Movie.title = "Movie 1", Movie.genres = []},
                         Movie.Movie {Movie.movieId = 102, Movie.title = "Movie 2", Movie.genres = []} ]
                       [ Tag {movieId = 101, userId = 1, tag = "tag_1", timestamp = 100} ]

    -- tf: tag_1: 1
    -- tfidf: log 2 - log 1 = 0.6931471805599453
    -- item_term: [(101, tag_1): 1]
    -- movieTFIDFs: [(101, tag_1): 1 * 0.6931471805599453]
    -- movieNormTFIDFs: [(101, tag_1): 0.6931471805599453 / 0.6931471805599453]

    let expectedSingle = Map.fromList [((101, "tag_1"), 1.0)]
    assertEqual "Expected map with 1 tag and 1 movie" expectedSingle singleTerm

    let coupleTerms = q [ Movie.Movie {Movie.movieId = 101, Movie.title = "Movie 1", Movie.genres = []}
                          , Movie.Movie {Movie.movieId = 102, Movie.title = "Movie 2", Movie.genres = []} ]
                       [ Tag {movieId = 101, userId = 1, tag = "tag_1", timestamp = 100}
                         , Tag {movieId = 101, userId = 1, tag = "tag_2", timestamp = 100}
                         , Tag {movieId = 102, userId = 2, tag = "tag_1", timestamp = 101}]

    -- tf: tag_1: 2
    --     tag_2: 1
    --
    -- tfidf tag_1: log 2 - log 2 = 0
    -- tfidf tag_2: log 2 - log 1 = 0.6931471805599453
    --
    -- item_term: [(101, tag_1): 1, (101, tag_2): 1, (102, tag_1): 1]
    -- movieTFIDFs: [(101, tag_1): 1 * 0]
    --              [(101, tag_2): 1 * 0.6931471805599453]
    --              [(102, tag_1): 1 * 0]
    -- movieNormTFIDFs: [(101, tag_1): 0.6931471805599453 / 0.9802581434685471]

    let expectedCouple = Map.fromList [((101, "tag_1"), 0.0), ((101, "tag_2"), 1.0), ((102, "tag_1"), 0.0)]
    assertEqual "Expected map with 2 tags and 1 movie" expectedCouple coupleTerms
    )

testProfiles = TestCase (
  do
    -- no rated movies = []
    let empty = Profile.profiles [] Map.empty
    assertEqual "Expected 0 for empty arguments"
                Map.empty
                empty

    -- 1 rated >= 3.5 2 movies with 2 tags
    let movieWith2Tags = Profile.profiles [ Rating.Rating { Rating.userId = 1
                                                          , Rating.movieId = 101
                                                          , Rating.rating = 4
                                                          , Rating.timestamp = 100 }
                                            , Rating.Rating { Rating.userId = 1
                                                              , Rating.movieId = 102
                                                              , Rating.rating = 4
                                                              , Rating.timestamp = 100 }]
                                          (Map.fromList [( (101, "tag_1"), 0.5 ),
                                                         ( (101, "tag_2"), 0.7 ),
                                                         ( (102, "tag_2"), 0.3 )])
    assertEqual "Expected map with TFIDFs from 2 terms for a single movie"
                (Map.fromList [( (1, "tag_1"), 0.5 ), ( (1, "tag_2"), 1.0 )])
                movieWith2Tags

    -- 1 rated < 3.5 movie with 1 tag
    -- 1 rated movie with 2 tags
    --
    let movieWith2Tags2 = Profile.profiles [ Rating.Rating { Rating.userId = 1
                                                          , Rating.movieId = 101
                                                          , Rating.rating = 4
                                                          , Rating.timestamp = 100 }
                                            -- this rating does not count because it is less then 3.5
                                            , Rating.Rating { Rating.userId = 1
                                                              , Rating.movieId = 102
                                                              , Rating.rating = 3
                                                              , Rating.timestamp = 100 }]
                                          (Map.fromList [( (101, "tag_1"), 0.5 ),
                                                         ( (101, "tag_2"), 0.7 ),
                                                         ( (102, "tag_2"), 0.3 )])
    assertEqual "Expected map with TFIDFs from 2 terms for a single movie"
                (Map.fromList [( (1, "tag_1"), 0.5 ), ( (1, "tag_2"), 0.7 )])
                movieWith2Tags2
    )

testItemScore = TestCase (
  do
    -- no rated movies = []
    let empty = Profile.item_score 1 101 Map.empty Map.empty
    assertEqual "Expected 0 for empty arguments"
                0
                empty

    -- 1 tag / 1 movie
    -- p_u . q_i = 0.5*0.4 = 0.2
    -- |p_2| * |q_i| = 0.5*0.4 = 0.2
    -- 0.2 / 0.2 = 1.0
    let single = Profile.item_score 1 101 (Map.fromList [( (1, "tag_1"), 0.5 )]) -- p
                                      (Map.fromList [( (101, "tag_1"), 0.4 )]) -- q

    assertEqual "1 tag / 1 movie"
                1.0
                single

    -- 1 movie has 2 existing tags and 1 missing tag
    -- p_u . q_i = 2*0.1 + 3*0.2 + 0.1*Nothing = 0.8
    -- |p_2| * |q_i| = sqrt(4+9+1)*sqrt(0.01+0.04) = 0.8366600265340756
    -- 0.8 / 0.2 = 0.9561828874675149
    let simple = Profile.item_score 1 101 (Map.fromList [( (1, "tag_1"), 2 )
                                                      , ( (1, "tag_2"), 3 )
                                                      , ( (1, "tag_3"), 1 )]) -- p
                                      (Map.fromList [( (101, "tag_1"), 0.1 )
                                                      , ( (101, "tag_2"), 0.2 )]) -- q

    assertEqual "1 movie has 2 existing tags and 1 missing tag"
                0.9561828874675149
                simple
    )

tests = TestList [TestLabel "Test TF" testTf
                  , TestLabel "Test TFIDF" testTfidf
                  , TestLabel "Test Movie term frequency" testItemTf
                  , TestLabel "Test Movie terms norm" testItemNorm
                  , TestLabel "Test Tags in Movies Frequency" testQ
                  , TestLabel "Test Profiles creation" testProfiles
                  , TestLabel "Test Item Scores" testItemScore
                  ]

main = (runTestTT tests)
