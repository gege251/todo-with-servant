export TPG_HOST=localhost
export TPG_DB=postgres
export TPG_USER=postgres
export TPG_PASS=

dev:
	stack build
	stack ghci servant-persistent-todoapi:exe:todoapi

build:
	stack build

gen-elm:
	stack build
	stack exec -- todoapi-gen-elm
	elm-format ../client/src/Requests --elm-version=0.19 --yes

gen-docs:
	stack build
	stack exec -- todoapi-gen-docs
