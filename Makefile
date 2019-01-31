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

_ESPIAL_PS_ID = $$(docker-compose ps -q espial)
_LOCAL_INSTALL_PATH = $$(stack path | grep local-install-root | awk -e '{print $$2}')
_EKG_ASSETS_PATH = $$(find .stack-work -type d | grep ekg.*assets)

docker-compose-build: build 
	@rm -Rf dist && mkdir -p dist
	@cp $(_LOCAL_INSTALL_PATH)/bin/* dist
	@cp -R static dist
	@rm -Rf dist/static/tmp
	@cp -R config dist
	@mkdir -p dist/ekg/assets
	@cp -R $(_EKG_ASSETS_PATH) dist/ekg
	@docker-compose build espial
docker-compose-up:
	@docker-compose up --no-deps --no-build espial
docker-compose-down:
	@docker-compose down
docker-compose-up-d:
	@docker-compose up --no-deps --no-build -d espial
docker-compose-pull:
	@docker-compose pull espial
docker-compose-push:
	@docker tag localhost/espial:espial $(HUB_REPO)/espial:espial
	@docker-compose push espial
docker-espial-logs:
	@docker logs -f --since `date -u +%FT%TZ` $(_ESPIAL_PS_ID)
docker-espial-shell:
	@$(docker_espial) sh


_HUB_REPO = ${HUB_REPO}
ifeq ($(_HUB_REPO),)
	_HUB_REPO := "localhost"
endif

docker_espial = docker-compose exec espial

clean:
	@stack clean
