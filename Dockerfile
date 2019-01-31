#-*- mode:conf; -*-

FROM jonschoning/espial:scratch
WORKDIR /app
COPY . .
CMD ./espial +RTS -T
