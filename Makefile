.PHONY: app
app:
	dune build bin/app.exe && cp -r _build/default/bin/app.exe app
