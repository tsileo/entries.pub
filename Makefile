.PHONY: app
app:
	dune build src/app.exe && cp -r _build/default/src/app.exe app
