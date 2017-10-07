FROM haskell

RUN cabal update && cabal install MissingH
RUN cabal update && cabal install HUnit
RUN cabal update && cabal install split
