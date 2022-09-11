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

docker-compose-build: build 
	@rm -Rf dist && mkdir -p dist
	@cp $(_LOCAL_INSTALL_PATH)/bin/* dist
	@cp -R static dist
	@rm -Rf dist/static/tmp
	@cp -R config dist
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
docker-espial-logs:
	@$(_DOCKER) logs -f --since `date -u +%FT%TZ` $(_ESPIAL_PS_ID)
docker-espial-shell:
	@$(docker_espial) sh


_HUB_REPO = ${HUB_REPO}
ifeq ($(_HUB_REPO),)
	_HUB_REPO := "localhost"
endif

docker_espial = $(_DOCKER_COMPOSE) exec espial

clean:
	@stack clean
