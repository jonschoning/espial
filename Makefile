_DOCKER:=docker
_DOCKER_COMPOSE:=docker compose

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
	@yesod devel

migrate-createdb:
	@stack exec migration -- createdb --conn espial.sqlite3

serve:
	@stack exec espial -- +RTS -T

_ESPIAL_PS_ID = $$($(_DOCKER_COMPOSE) ps -q espial)
_LOCAL_INSTALL_PATH = $$(stack path | grep local-install-root | awk -e '{print $$2}')
_DOCKER_COMPOSE_ARCHIVEBOX07 = $(_DOCKER_COMPOSE) -f docker-compose.archivebox07.yml

docker-compose-build: build 
	@$(_DOCKER_COMPOSE) build espial
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
