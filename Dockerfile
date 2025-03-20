FROM node:22.14-slim AS web

WORKDIR /app

COPY ./assets ./assets
COPY ./web ./web

WORKDIR /app/web
RUN apt-get clean && apt-get update && apt-get upgrade -y && apt-get install -y python3 build-essential

RUN --mount=type=cache,id=npm,target=/root/.npm npm ci

RUN npm run build

FROM haskell:9.10-slim-bullseye AS build

WORKDIR /opt/server

RUN cabal update
RUN apt-get clean && apt-get update
RUN apt-get install -y locales

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

COPY package.yaml placewaifu.cabal ./
RUN --mount=type=cache,id=cabal-deps,target=/root/.cabal cabal build --only-dependencies -j4

COPY ./ ./
RUN --mount=type=cache,id=cabal,target=/root/.cabal cabal install

FROM ubuntu

COPY --from=web /app/web/dist ./web/dist
COPY --from=build /root/.local/bin/placewaifu .
COPY ./assets ./assets

USER nobody

EXPOSE 1234
CMD ["./placewaifu", "--assets=./assets"]
