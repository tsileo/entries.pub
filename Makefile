.PHONY: app
app:
	dune build app.exe && cp -r _build/default/app.exe app
