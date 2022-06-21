FROM haskell:9

WORKDIR /opt/parallel

RUN cabal update && cabal install --lib parallel

COPY ./Parallel_Basics.hs /opt/parallel/main.hs


