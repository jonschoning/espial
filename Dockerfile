FROM fpco/stack-build:lts-24.37 AS builder

WORKDIR /src

COPY . .
RUN stack setup
RUN stack build --copy-bins --local-bin-path /opt/espial/bin


FROM debian:bookworm-slim AS runtime-deps

ARG BUSYBOX_VERSION=1.36.1

RUN apt-get update \
  && apt-get install -y --no-install-recommends build-essential bzip2 ca-certificates libgmp10 wget zlib1g \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp

RUN wget -O busybox.tar.bz2 "https://busybox.net/downloads/busybox-${BUSYBOX_VERSION}.tar.bz2" \
  && tar -xf busybox.tar.bz2 \
  && mv "busybox-${BUSYBOX_VERSION}" busybox

WORKDIR /tmp/busybox

RUN make allnoconfig \
  && sed -i 's/^# CONFIG_STATIC is not set$/CONFIG_STATIC=y/' .config \
  && sed -i 's/^# CONFIG_FEATURE_PREFER_APPLETS is not set$/CONFIG_FEATURE_PREFER_APPLETS=y/' .config \
  && sed -i 's/^# CONFIG_FEATURE_SH_STANDALONE is not set$/CONFIG_FEATURE_SH_STANDALONE=y/' .config \
  && sed -i 's/^# CONFIG_CAT is not set$/CONFIG_CAT=y/' .config \
  && sed -i 's/^# CONFIG_CP is not set$/CONFIG_CP=y/' .config \
  && sed -i 's/^# CONFIG_LS is not set$/CONFIG_LS=y/' .config \
  && sed -i 's/^# CONFIG_MKDIR is not set$/CONFIG_MKDIR=y/' .config \
  && sed -i 's/^# CONFIG_RM is not set$/CONFIG_RM=y/' .config \
  && sed -i 's/^# CONFIG_SH_IS_ASH is not set$/CONFIG_SH_IS_ASH=y/' .config \
  && yes '' | make oldconfig \
  && make -j"$(nproc)" \
  && mkdir -p /opt/runtime-bin \
  && cp busybox /opt/runtime-bin/busybox \
  && ln -sf busybox /opt/runtime-bin/sh \
  && ln -sf busybox /opt/runtime-bin/cp \
  && ln -sf busybox /opt/runtime-bin/ls \
  && ln -sf busybox /opt/runtime-bin/cat \
  && ln -sf busybox /opt/runtime-bin/mkdir \
  && ln -sf busybox /opt/runtime-bin/rm


FROM gcr.io/distroless/base-debian12 AS runtime

WORKDIR /app

COPY --from=runtime-deps /opt/runtime-bin/ /bin/
COPY --from=runtime-deps /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/
COPY --from=runtime-deps /usr/lib/x86_64-linux-gnu/libz.so.1 /usr/lib/x86_64-linux-gnu/
COPY --from=builder /src/config ./config
COPY --from=builder /src/static ./static
COPY --from=builder /opt/espial/bin/espial ./espial
COPY --from=builder /opt/espial/bin/migration ./migration

ENTRYPOINT []
CMD ["./espial", "+RTS", "-T"]