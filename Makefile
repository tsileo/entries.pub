.PHONY: app
app:
	dune build bin/app.exe && cp -r _build/default/bin/app.exe app

.PHONY: bundle
bundle:
	tar czvf bundle.tgz app static template.html atom.xml config.yaml
