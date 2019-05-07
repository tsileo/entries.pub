# entries.pub

[![Build Status](https://d.a4.io/api/badges/tsileo/entries.pub/status.svg)](https://d.a4.io/tsileo/entries.pub)

WIP IndieWeb blog engine

## Features

 - Minimalist
   - no JavaScript
   - no admin UI (rely on Micropub)
 - Data is stored in a local Git repo (using [Irmin](https://github.com/mirage/irmin))
   - backing up data is easy
   - every mutations is a commit
 - [CLI client](https://github.com/tsileo/entries)
 - Microformats aware (exports `h-feed`, `h-entry`, `h-card`, ...)
 - Micropub support
 - WebSub
   - for the Atom feed
   - and the JSON feed
 - Webmention support
 - Syndicate to ActivityPub via [microblog.pub](https://github.com/tsileo/microblog.pub)


## In order to support TLS 1.3

    $ opam pin add ssl 0.5.5
    $ export CONDUIT_TLS=openssl

