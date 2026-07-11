.PHONY: clean typecheck install bundle lint

all: bundle

install: 
	@npm ci

typecheck: 
	@npm run typecheck

lint: 
	@npm run lint

build: typecheck lint
	@npm run bundle:build

bundle: typecheck lint
	@npm run bundle

clean:
	rm -f dist/*