FROM fpco/stack-build:lts-24.37 AS builder

WORKDIR /src

COPY . .
RUN stack setup
RUN stack build --copy-bins --local-bin-path /opt/espial/bin


FROM jonschoning/espial:scratch AS runtime

WORKDIR /app

COPY --from=builder /src/config ./config
COPY --from=builder /src/static ./static
COPY --from=builder /opt/espial/bin/espial ./espial
COPY --from=builder /opt/espial/bin/migration ./migration

CMD ["./espial", "+RTS", "-T"]