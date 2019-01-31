.PHONY: clean build

all: build

install: 
	spago install

build: 
	@spago build
	@spago bundle --to dist/app.js
	@(cd dist && terser app.js -m -c -o app.min.js)
	@rm -f dist/*.gz
	@gzip -k dist/app.js
	@gzip -k dist/app.min.js
	@find dist -type f -printf "%kK\\t%h/%f\\n" | sort -k 2
	@cp dist/app.js ../static/js/app.js
	@cp dist/app.js.gz ../static/js/app.js.gz
	@cp dist/app.min.js ../static/js/app.min.js
	@cp dist/app.min.js.gz ../static/js/app.min.js.gz

docs:
	@rm -Rf generated-docs
	@purs docs ".spago/*/*/src/**/*.purs" --format html

clean:
	rm -f dist/*

# inotifywait -m -r -q -e close_write --format '%T %w%f' --timefmt '%T' src | while read FILE; do echo $FILE; make; done
