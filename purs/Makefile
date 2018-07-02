o_dist = dist
o_name = app.js
m_name = app.min.js
y_name = app.js
y_static = ../static/js
pulp = pulp --psc-package 
modules = App,BMark,Globals
skip = --skip-entry-point 

minify = uglifyjs

.PHONY: clean build

all: deploy

build: 
	$(pulp) build -O $(skip) --modules $(modules) --to $(o_dist)/$(o_name) \
        && $(minify) $(o_dist)/$(o_name) > $(o_dist)/$(m_name)

watch:
	$(pulp) -w build -O $(skip) --modules $(modules) --to $(o_dist)/$(o_name)

deploy: build
	cp $(o_dist)/$(o_name) $(y_static)/$(y_name)

docs:
	$(pulp) docs -- --format html

clean:
	rm -f dist/*
