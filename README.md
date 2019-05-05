# entries.pub

[![Build Status](https://d.a4.io/api/badges/tsileo/entries.pub/status.svg)](https://d.a4.io/tsileo/entries.pub)

WIP IndieWeb blog engine

## Features

 - Minimalist
   - No JavaScript
   - No admin UI (rely on Micropub)
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

