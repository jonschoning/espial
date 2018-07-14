o_dist = dist
o_name = app.js
m_name = app.min.js
y_name = app.js
y_static = ../static/js
pulp = pulp --psc-package
modules = --modules App,User,Globals,Util
skip = --skip-entry-point 

minify = uglifyjs

.PHONY: clean build

all: deploy

build: 
	$(pulp) build -O \
		$(skip) \
		$(modules) \
		--to $(o_dist)/$(o_name) && \
		$(minify) $(o_dist)/$(o_name) > $(o_dist)/$(m_name) && \
		rm -f $(o_dist)/$(m_name).gz && \
		gzip -k $(o_dist)/$(m_name) && \
		ls -lh $(o_dist)

watch:
	$(pulp) -w build -O $(skip) $(modules) --to $(o_dist)/$(o_name)

deploy: build
	cp $(o_dist)/$(m_name) $(y_static)/$(y_name) && \
	cp $(o_dist)/$(m_name).gz $(y_static)/$(y_name).gz

docs:
	$(pulp) docs -- --format html

repl:
	$(pulp) repl

clean:
	rm -f dist/*
