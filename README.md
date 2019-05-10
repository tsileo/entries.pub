# entries.pub

[![Build Status](https://d.a4.io/api/badges/tsileo/entries.pub/status.svg)](https://d.a4.io/tsileo/entries.pub)

WIP IndieWeb blog engine

## Features

 - Minimalist
   - no JavaScript
   - no admin UI (rely on Micropub)
   - entries Markdown formatted
 - Data is stored in a local Git repo (using [Irmin](https://github.com/mirage/irmin))
   - backing up data is easy
   - every mutations is a commit
 - Microformats aware (exports `h-feed`, `h-entry`, `h-card`, ...)
 - Atom and [JSON](https://jsonfeed.org/) feed
 - [IndieWeb](https://indieweb.org/) support
   - [Micropub](https://www.w3.org/TR/micropub/)
     - Simple [CLI client](https://github.com/tsileo/entries)
   - [WebSub](https://www.w3.org/TR/websub/)
     - for the Atom feed
     - and the JSON feed
   - [Webmention](https://www.w3.org/TR/webmention/)


## In order to support TLS 1.3

    $ opam pin add ssl 0.5.5
    $ export CONDUIT_TLS=openssl
