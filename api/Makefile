dev:
	stack build
	stack exec -- todoapi

gen-elm:
	stack build
	stack exec -- todoapi-gen-elm
	elm-format ../client/src/Requests --elm-version=0.19 --yes

gen-docs:
	stack build
	stack exec -- todoapi-gen-docs