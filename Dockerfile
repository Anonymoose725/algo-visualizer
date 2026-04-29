FROM haskell:9.6.7

WORKDIR /app

COPY algo-visualizer.cabal .
RUN cabal update && cabal build --only-dependencies

COPY . .
RUN cabal build
RUN cabal install --install-method=copy --installdir=/app/bin

EXPOSE 8080

CMD ["/app/bin/algo-visualizer"]
