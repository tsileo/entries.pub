# entries.pub

[![Build Status](https://d.a4.io/api/badges/tsileo/entries.pub/status.svg)](https://d.a4.io/tsileo/entries.pub)

WIP IndieWeb blog engine.

You can see it running here: [https://hexa.ninja](https://hexa.ninja).

## Features

 - Minimalist
   - no JavaScript
   - no admin UI (rely on Micropub)
   - entries are Markdown formatted
 - Special "Pages" section
   - using the `page` category, for timeless content
 - Data is stored in a local Git repo (using [Irmin](https://github.com/mirage/irmin))
   - backing up data is easy (push to a remote)
   - every mutation is recorded in a commit
 - Microformats aware (exports `h-feed`, `h-entry`, `h-card`, ...)
 - Atom and [JSON](https://jsonfeed.org/) feed
 - [IndieWeb](https://indieweb.org/) support
   - [Micropub](https://www.w3.org/TR/micropub/)
     - for the "admin"
     - there's a [CLI client](https://github.com/tsileo/entries), but any Micropub client will do
   - [WebSub](https://www.w3.org/TR/websub/)
     - for the Atom feed
     - and the JSON feed
     - uses https://pubsubhubbub.appspot.com/ by default
   - [Webmention](https://www.w3.org/TR/webmention/)
     - received webmention are automatically displayed (links only)

### Development

### In order to support TLS 1.3

    $ opam pin add ssl 0.5.5
    $ export CONDUIT_TLS=openssl
