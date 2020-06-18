# Arew Scheme

![Louvre Abu Dhabi](https://git.sr.ht/~amirouche/arew/blob/master/alvaro-pinot-czDvRp5V2b0-unsplash.jpg)

## Kesako?

Arew Scheme wants to be an easy to install, easy to use Scheme
implementation of [R7RS](https://r7rs.org) standard.  It rely on Chez
Scheme compiler that is a R6RS implementation.

Simply said, it is Chez Scheme with R7RS libraries and a shim to
support R7RS `define-library` form.  Eventually, we want to support
all of R7RS.

Toward that goal, we set the goal to draw the line forward production
use, by re-implementing and improving
[copernic](https://github.com/amirouche/copernic/) using Arew in the
backend and [Chibi](https://github.com/ashinn/chibi-scheme/) in the
frontend.

copernic wants to scale up and scale down [wikidata
platform](https://addshore.com/2020/01/wbstack-infrastructure/). To
summarize, it is a social network geared toward the creation,
maintenance, and use of a knowledge base that is bigger than memory.
The added value compared to wikidata platform is four sides:

- Scale up, thanks to foundationdb one can scale both in terms of data
  size, number of simple queries, and background queries.

- Scale down, thanks to foundationdb (again), one can scale down the
  wikidata platform to work on a single machine.

- Because the data is versioned, it allows to do time travelling
  queries, and implement pull-request mechanic.

- Drop legacy software.

There is four, possibly five sides to this a project:

- Product involves defining the features of copernic, the basics are
  explained above. But perfection lies in the details.

- Frontend implement the graphical user interface (webui), as of right
  now we use chibi scheme, but we might move to gambit or something
  based on nanopass framework.  The target webui is a single page
  application. The frontend part will be based on [a
  prototype](https://amirouche.github.io/scheme-comparator/) and [a
  paper submitted at Scheme Workshop
  2020](https://github.com/amirouche/nightwatch). See way below for
  the "sitemap".

- Backend and backing store: That is the code running in the server,
  it involves finishing the foundationdb bindings (sync and async),
  re-implement
  [`vnstore`](https://github.com/amirouche/copernic/blob/master/copernic/vnstore.py),
  build a basic http server, sxml support, REST interface, most likely
  [full-text search engine](https://github.com/amirouche/babelia).
  It will also require auth, possibly social auth and others.

- Scraper: To be able to take part in EU Datathon, we need, European
  data. Finding european data is left as an exercices to the reader.
  The difficult part is fetching the data (requires an http client),
  then parsing (requires csv, tsv, possibly more scary formats), and
  the best of all is that we need to make a difference between what is
  in the database and what is new/removed/changed in the dataset
  because they do not track that upstream.

- Monitoring: We can possibly use off-the-shelf solution like
  prometheus or clickhouse or ELK.  My favorite solution would be to
  study those softwares and re-implement a subset that is good enough.
  The goal is to monitor the foundationdb cluster and the backend
  processus and possibly the frontend to be able to be alerted in case
  of errors before they happen.

The current "sitemap" looks like the following:

```org-mode
** Basics
*** Ask a (natural language) question, prolly get an answer
*** Learn how to query the knowledge base programmatically
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
part, similar to a microblogging application might be implemented
later.
