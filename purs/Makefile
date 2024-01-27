.PHONY: clean build

all: bundle

install: 
	@npm run spago install

build: 
	@npm run spago build

bundle: build
	npm run esbuild-bundle-min
	cp -R dist ../static/js/purs

docs:
	@rm -Rf generated-docs
	@npm run purs -- docs ".spago/*/*/src/**/*.purs" --format html

docset: docs
	@(cd docset; python3 ./gen-docset.py)
clean:
	rm -f dist/*

# inotifywait -m -r -q -e close_write --format '%T %w%f' --timefmt '%T' src | while read FILE; do echo $FILE; make build; done
