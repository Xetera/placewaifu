FROM haskell:8.6.5 as build

WORKDIR /opt/server
COPY placewaifu.cabal package.yaml stack.yaml* ./
RUN stack install --only-dependencies

COPY ./ ./
RUN stack install

FROM ubuntu

COPY --from=build /root/.local/bin/placewaifu .
COPY ./assets ./assets
CMD ./pacewaifu --assets=./assets