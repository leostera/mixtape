BIN=./node_modules/.bin

all: lint build

deps:
	yarn

build:
	$(BIN)/browserify index.js > _build.js
	gzip -c -9 _build.js > _build.js.gz

lint:
	$(BIN)/standard index.js

fix:
	$(BIN)/standard --fix index.js

cleanall:
	rm -rf node_modules _build.js
