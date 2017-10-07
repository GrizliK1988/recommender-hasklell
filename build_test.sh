#!/bin/sh

docker run \
  -it \
  --rm \
  --name recommender-app \
  -v ${PWD}:/app \
  -w="/app/week4" \
  recommder-haskell:latest \
  /bin/bash -c \
  "ls; ghc --make unit_test.hs; ./unit_test"
