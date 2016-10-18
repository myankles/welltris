SRC_FILES = $(shell find src -iname '*.elm')

deploy/welltris.js: $(SRC_FILES)
	elm-make src/main.elm --output deploy/welltris.js 2>&1

clean:
	rm -fv deploy/welltris.js
