#!/bin/sh

docker run \
  -it \
  --rm \
  --name recommender-app \
  -v ${PWD}:/app \
  -w="/app/week4" \
  recommder-haskell:latest \
  /bin/bash #-c \
#  "ghc --make tf_idf_recommender.hs; ./tf_idf_recommender"
