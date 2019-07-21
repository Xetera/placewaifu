FROM haskell:8.6.5 as build

WORKDIR /opt/server
RUN apt-get clean && apt-get update

RUN curl -sL https://deb.nodesource.com/setup_10.x | bash - && apt-get install -y nodejs locales

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY placewaifu.cabal package.yaml stack.yaml* ./
COPY .stack-work ./stack-work
RUN stack install --only-dependencies

COPY ./ ./
RUN stack install

RUN cd web && npm run build

FROM ubuntu

COPY --from=build /root/.local/bin/placewaifu .
COPY --from=build /opt/server/web/dist ./web/dist
COPY ./assets ./assets
CMD ./placewaifu --assets=./assets
