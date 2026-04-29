FROM haskell:9.6.7

WORKDIR /app

COPY algo-visualizer.cabal .
RUN cabal update && cabal build --only-dependencies

COPY . .
RUN cabal build

RUN cp $(cabal list-bin algo-visualizer) app/algo-visualizer-exe

EXPOSE 8080

CMD ["/app/algo-visualizer-exe"]
