export TPG_DB=postgres
export TPG_USER=postgres
export TPG_PASS=

dev:
	stack build --fast --file-watch --exec "bash -c \"pkill todoapi; stack exec todoapi &\""

build:
	stack build

gen-elm:
	stack build --exec todoapi-gen-elm
	elm-format ../client/src/Requests --elm-version=0.19 --yes

gen-docs:
	stack build --exec todoapi-gen-docs

gen-swagger:
	stack build --exec todoapi-gen-swagger
