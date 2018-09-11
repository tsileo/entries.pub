.PHONY: app
app:
	ocamlfind ocamlopt app.ml -o app -package irmin-unix,lwt.unix,yurt -linkpkg
