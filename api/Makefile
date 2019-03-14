dev:
	stack build --exec todoapi

build: 
	stack build

gen-elm:
	stack build --exec todoapi-gen-elm
	elm-format ../client/src/Requests --elm-version=0.19 --yes

gen-docs:
	stack build --exec todoapi-gen-docs