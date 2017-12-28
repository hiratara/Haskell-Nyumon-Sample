FROM fpco/stack-build:lts-8.24

RUN apt-get update \
 && apt-get install -y sqlite3 libsqlite3-dev

RUN groupadd -r weight \
 && useradd -ms /bin/bash -g weight weight \
 && mkdir /work /data \
 && chown -R weight:weight /work /data

USER weight
ENV LANG C

# To cache .stack-work
RUN stack --system-ghc --resolver lts-8.24 setup
RUN stack --system-ghc --resolver lts-8.24 build \
    bcrypt \
    bytestring \
    directory \
    filepath \
    HDBC \
    HDBC-sqlite3 \
    HUnit \
    http-types \
    Spock \
    mustache \
    optparse-applicative \
    persistable-record \
    process \
    relational-query-HDBC \
    relational-query \
    resource-pool \
    safe-exceptions \
    template-haskell \
    temporary \
    text \
    time \
    transformers \
    utf8-string \
    wai \
    wai-extra

ENV WEIGHT_DB_PATH /data/weight.db
WORKDIR /work

COPY --chown=weight:weight . /work

RUN cat chap10-samples/data/schema.sql | sqlite3 /data/weight.db \
 && stack --system-ghc build --test weight-recorder

ENTRYPOINT ["stack", "--system-ghc"]

EXPOSE 8080

CMD ["exec", "weight-recorder", "--", "--db", "\"/data/weight.db\"", "--port", "8080"]
