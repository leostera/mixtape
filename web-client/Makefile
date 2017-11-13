BIN=./node_modules/.bin

all: lint build

build:
	$(BIN)/browserify index.js > _build.js

lint:
	$(BIN)/standard index.js

fix:
	$(BIN)/standard --fix index.js
