.PHONY: clean build

all: bundle

install: 
	@npm ci

build: 
	@npm run typecheck

bundle: build
	@npm run esbuild-bundle
	@npm run esbuild-bundle-min
	@rm -f dist/*.gz
	@gzip -k dist/app.min.js.map
	@gzip -k dist/app.min.js
	@find dist -type f -printf "%kK\\t%h/%f\\n" | sort -k 2
	@cp dist/app.min.js ../static/js/app.min.js
	@cp dist/app.min.js.gz ../static/js/app.min.js.gz
	@cp dist/app.min.js.map ../static/js/app.min.js.map
	@cp dist/app.min.js.map.gz ../static/js/app.min.js.map.gz

clean:
	rm -f dist/*