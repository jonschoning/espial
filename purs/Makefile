o_dist = dist
o_name = app.js
m_name = app.min.js
y_name = app.min.js
y_static = ../static/js
pulp = pulp --psc-package
modules = --modules App,User,Globals,Util
skip = --skip-entry-point 

.PHONY: clean build

all: deploy

build: 
	$(pulp) build -O \
		$(skip) \
		$(modules) \
		--to $(o_dist)/$(o_name) && \
		(cd $(o_dist) && terser $(o_name) -m -c -o $(m_name)) && \
		rm -f $(o_dist)/*.gz && \
		gzip -k $(o_dist)/$(o_name) && \
		gzip -k $(o_dist)/$(m_name) && \
		ls -lh $(o_dist)

watch:
	$(pulp) -w build -O $(skip) $(modules) --to $(o_dist)/$(o_name)

deploy: build
	cp $(o_dist)/$(o_name) $(y_static)/$(o_name) && \
	cp $(o_dist)/$(o_name).gz $(y_static)/$(o_name).gz && \
	cp $(o_dist)/$(m_name) $(y_static)/$(y_name) && \
	cp $(o_dist)/$(m_name).gz $(y_static)/$(y_name).gz

docs:
	$(pulp) docs -- --format html

repl:
	$(pulp) repl

clean:
	rm -f dist/*
