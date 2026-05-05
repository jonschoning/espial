.PHONY: clean typecheck install bundle

all: bundle

install: 
	@npm ci

typecheck: 
	@npm run typecheck

build: typecheck
	@npm run bundle:build

bundle: typecheck
	@npm run bundle

clean:
	rm -f dist/*