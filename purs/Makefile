.PHONY: clean build

all: build

install: 
	spago install

build: 
	@spago build
	@spago bundle-app --to dist/bundle.js
	@npm run parcel-build
	@rm -f dist/*.gz
	@gzip -k dist/app.min.js.map
	@gzip -k dist/app.min.js
	@find dist -type f -printf "%kK\\t%h/%f\\n" | sort -k 2
	@cp dist/app.min.js.map ../static/js/app.min.js.map
	@cp dist/app.min.js.map.gz ../static/js/app.min.js.map.gz
	@cp dist/app.min.js ../static/js/app.min.js
	@cp dist/app.min.js.gz ../static/js/app.min.js.gz

docs:
	@rm -Rf generated-docs
	@purs docs ".spago/*/*/src/**/*.purs" --format html

docset: docs
	@(cd docset; python3 ./gen-docset.py)
clean:
	rm -f dist/*

# inotifywait -m -r -q -e close_write --format '%T %w%f' --timefmt '%T' src | while read FILE; do echo $FILE; make; done
