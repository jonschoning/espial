_DOCKER:=docker
_DOCKER_COMPOSE:=docker compose

VERSION := $(shell sed -n 's/^version:[[:space:]]*//p' espial.cabal)
GIT_SHA := $(shell git rev-parse --short HEAD 2>/dev/null || echo UNKNOWN)
BUILD_DATE := $(shell date -u +%Y-%m-%dT%H:%M:%SZ)
SOURCE_URL := $(shell sed -n 's/^homepage:[[:space:]]*//p' espial.cabal)
TITLE := $(shell sed -n 's/^name:[[:space:]]*//p' espial.cabal)
DESCRIPTION := $(shell sed -n 's/^synopsis:[[:space:]]*//p' espial.cabal)
LICENSES := $(shell sed -n 's/^license:[[:space:]]*//p' espial.cabal)

.PHONY: clean build

all: build

build: 
	@stack build

build-fast: 
	@stack build --fast

build-watch:
	@stack build --file-watch --fast --ghc-options=-fno-code 

repl: 
	@stack ghci --test --bench --ghci-options=-fno-code --main-is=espial:exe:espial

ghcid:
	@ghcid -c "stack ghci --test --bench --ghci-options=-fno-code --main-is=espial:exe:espial"

devel:
	@stack exec -- yesod devel

serve:
	@stack exec espial -- +RTS -T

_ESPIAL_PS_ID = $$($(_DOCKER_COMPOSE) ps -q espial)
_LOCAL_INSTALL_PATH = $$(stack path | grep local-install-root | awk -e '{print $$2}')
_DOCKER_COMPOSE_ARCHIVEBOX07 = $(_DOCKER_COMPOSE) -f docker-compose.archivebox07.yml

docker-compose-build: build 
	@$(_DOCKER_COMPOSE) build espial \
		--build-arg VERSION=$(VERSION) \
		--build-arg GIT_SHA=$(GIT_SHA) \
		--build-arg BUILD_DATE=$(BUILD_DATE) \
		--build-arg SOURCE_URL="$(SOURCE_URL)" \
		--build-arg TITLE="$(TITLE)" \
		--build-arg DESCRIPTION="$(DESCRIPTION)" \
		--build-arg LICENSES="$(LICENSES)"
docker-compose-build-buildx: build
	@$(_DOCKER) buildx build -f Dockerfile.buildkit -t localhost/espial:espial --load \
		--build-arg VERSION=$(VERSION) \
		--build-arg GIT_SHA=$(GIT_SHA) \
		--build-arg BUILD_DATE=$(BUILD_DATE) \
		--build-arg SOURCE_URL="$(SOURCE_URL)" \
		--build-arg TITLE="$(TITLE)" \
		--build-arg DESCRIPTION="$(DESCRIPTION)" \
		--build-arg LICENSES="$(LICENSES)" \
		.
docker-compose-up:
	@$(_DOCKER_COMPOSE) up --no-deps --no-build espial
docker-compose-down:
	@$(_DOCKER_COMPOSE) down
docker-compose-up-d:
	@$(_DOCKER_COMPOSE) up --no-deps --no-build -d espial
docker-compose-pull:
	@$(_DOCKER_COMPOSE) pull espial
docker-compose-push:
	@docker tag localhost/espial:espial $(HUB_REPO)/espial:espial
	@$(_DOCKER_COMPOSE) push espial
docker-compose-push-beta:
	@docker tag localhost/espial:espial $(_HUB_REPO)/espial:beta
	@$(_DOCKER) push $(_HUB_REPO)/espial:beta
docker-espial-logs:
	@$(_DOCKER) logs -f --since `date -u +%FT%TZ` $(_ESPIAL_PS_ID)
docker-espial-shell:
	@$(docker_espial) sh

# commands for archive-backend: "archivebox07" 
docker-compose-up-archivebox07:
	@$(_DOCKER_COMPOSE_ARCHIVEBOX07) up --no-build
docker-compose-up-d-archivebox07:
	@$(_DOCKER_COMPOSE_ARCHIVEBOX07) up --no-build -d
docker-compose-down-archivebox07:
	@$(_DOCKER_COMPOSE_ARCHIVEBOX07) down

_HUB_REPO = ${HUB_REPO}
ifeq ($(_HUB_REPO),)
	_HUB_REPO := "localhost"
endif

docker_espial = $(_DOCKER_COMPOSE) exec espial

clean:
	@stack clean
