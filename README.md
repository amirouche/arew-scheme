# Arew Scheme

![Louvre Abu Dhabi](https://git.sr.ht/~amirouche/arew/blob/master/alvaro-pinot-czDvRp5V2b0-unsplash.jpg)

## Kesako?

Arew Scheme wants to be an easy to install, easy to use Scheme
implementation of [R7RS](https://r7rs.org) standard.  It rely on Chez
Scheme compiler that is a R6RS implementation.

Simply said, it is Chez Scheme with R7RS libraries and a shim to
support R7RS `define-library` form.

Toward that, we set the goal to draw the line forward production use,
by re-implementing and improving
[copernic](https://github.com/amirouche/copernic/) using Arew in the
backend and [Chibi](https://github.com/ashinn/chibi-scheme/) in the
frontend.

copernic wants to scale up and scale down [wikidata
platform](https://addshore.com/2020/01/wbstack-infrastructure/). To
summarize, it is a social network geared toward the creation,
maintenance, and use of a knowledge base that is bigger than memory.
The added value compared to the current wikidata platform is four
sides:

- Scale up, thanks to FoundationDB one can scale both in terms of data
  size, number of simple queries, and background queries.

- Scale down, thanks to FoundationDB (again), one can scale down the
  wikidata platform to work on a single machine.

- Because the data is versioned, it allows to do time travelling
  queries, and pull-requests.

- Drop legacy software.

There is four, possibly five sides to this a project:

- User interaction involves defining the features of copernic, and how
  they relate.  The basics are explained above. Perfection lies in the
  details.

- Frontend implement the graphical user interface (webui), as of right
  now we use chibi scheme, but we might move to gambit or something
  based on nanopass framework.  The target webui is a single page
  application. The frontend part will be based on [a
  prototype](https://amirouche.github.io/scheme-comparator/) and [a
  paper (wip)](https://github.com/amirouche/nightwatch). See way below
  for the "sitemap".

- Backend and backing store: That is the code running in the server,
  it involves finishing the FoundationDB bindings (sync and async),
  re-implement
  [`vnstore`](https://github.com/amirouche/copernic/blob/master/copernic/vnstore.py),
  build a basic HTTP server, sxml support, REST interface, most likely
  [full-text search engine](https://github.com/amirouche/babelia).  It
  will also require auth, possibly social auth and others.

- Scraper: To be able to take part in EU Datathon, we need european
  data. Finding european data is left as an exercices to the reader.
  The difficult part is fetching the data (requires an HTTP client),
  then parsing (requires csv, tsv, possibly more scary formats), and
  the best of all is that we need to make the difference between what
  is in the database and what is new/removed/changed in the dataset
  because they do not track that upstream.

- Monitoring: We can possibly use off-the-shelf solution like
  prometheus or clickhouse or ELK.  My favorite solution would be to
  study those softwares and re-implement a subset that is good enough.
  The goal is to monitor the FoundationDB cluster and the backend
  processus and possibly the frontend to be able to be alerted in case
  of errors.  Anyway, it must be open-source.

The current "sitemap" looks like the following:

```org-mode
** Basics
*** Ask a (natural language) question, prolly get an answer
*** Learn how to query the knowledge base programmatically (SPARQL tutorial)
*** Request for change
** Developer
*** Generate API key
*** List API key
*** Delete API key
*** API Documentation
**** CRUD changes
**** Changes feed
** Community
*** Post a message
*** Popular messages
*** Hashtags
*** Reactions
*** Follow user
*** Follow a change request
*** Follow a hashtag
*** Local timeline
*** Your timeline
*** Changes timeline
** Moderation
*** Kick
*** Ban
*** Invite
*** Mute
*** Block
*** Report
*** Review and validate changes
** Profile / User
*** Change password
*** Change email
*** Change bio
** Meta
*** Natural language query analysis
```

That list of features will be subject to changes.  Also the community
part, similar to a microblogging application, might be implemented
later.

## Why completly avoid SQL?

Copies.  The Python/Django experiment showed that it is necessary, to
copy data between FoudationDB and the SQL database.  That is neat for
a quick prototype.  Anyway, using a SQL database in the system, would
necessarly increase the number of processus that must be setup and
monitored. Also you would loose transactions and scalability.

The drawback is that it requires to re-implement some complex features
like full-text search (FTS) or geospatial queries.

Note: Full-text search requires to be able to do a map-reduce very
quickly. That is, there is necessarly some parallel work happening.

## Why rewrite copernic with Arew?

Because of the Global Interpreter Lock (GIL).  CPython is not really
suited for doing work in parallel, instead you have to rely on
multiple processus.  That leads willy-nilly to microservice
architecture, which itself leads to complex production operations.
The microservice architecture is fine when there is an improvement
over the monolith approach, eg. when you need more performance in a
particular part of your system and need to tape another programming
language in your system that you can not easily use otherwise.  Arew
does not have a GIL. It is possible to spawn a processus, and work
with multiple threads.
