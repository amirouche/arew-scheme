# Babelia User Manual

## Introduction

Babelia is a privacy friendly, decentralized, open source, and
accessible search engine.

Search has been an essential part of knowledge acquisition from the
dawn of time, whether it is antique lexicographically ordered filing
cabinets or nowadays computer-based wonders such as Google or
Bing. From casual search to help achieve common tasks such as cooking,
keeping up with the news, a regular dose of cat memes or professional
search such as science research. Search is, and will remain, an
essential daily-use tool, that stires human progress forward.

Babelia aims to replace the use of privateer search engines with a
search engine that is open, hence under the control of the commons.

Eventually, Babelia may offer features commonly associated with
existing search engines, such as maps, mail accounts, calendars, image
and video galleries, and a feed reader inside a single software.
Babelia wants to be an easy to install, easy to use, easy to maintain,
no-code, personal search engine that can scale to billions of
documents, beyond a terabyte of text data, for under €100 a month per
Babelia instance.

## The input box

The gist of textual search is the input box, it allows to ask and
hopefully find what your are looking for.  Like other popular search
engine, Babelia comes with mini-language that allows to express what
you are looking for.

Upon a query, babelia will return what is called **hits** those are
the documents that match the query you entered in the input box,
relying on the following rules:

1. All words separated by space will appear *verbatim* in
   hits. Punctuation characters are ignored e.g. dash `-` or `,` or
   `;` except the exclamation mark, dash `-` and minus `,` as
   explained in the following rules.

2. Two or more words separated by space and wrapped in double quotes
   will appear *verbatim* in hits e.g. `"search engine tooling"`

3. A dash character just before a word without space such as
   `-privateer` will yield all hits that match the rest of the query
   but do not contain the word `privateer`. Such query terms are
   called negative query terms.

4. When `,latest` appears in the query, instead of sorting by
   relevance, hits will be sorted according to the time they were
   indexed for the first time.  It allows to retrieve new or recent
   documents.

5. Up to three exclamation marks can appear in a query. The more
   exclamation marks they are the more Babelia will consider words
   related to the words present in the query to score and sort the
   hits. In practice, it may yield document where the exact words of
   the query never appear as-is. This rule does not change the second
   rule, in particular `search engine "tooling" !!` means `toooling`
   must appear verbatim in hits, whereas `search` might appear as
   `searching`. Exclamation mark allows to extend the breath of the
   result, it may or may not yield better results depending on your
   intent.

6. When there is three exclamation marks in the query, Babelia will
   also consider related negative words and reduce the score of
   documents that contains words related to the negated word. For
   instance, given the following `search engine -privateer !!!` will
   yield document that necessarly contain `search` and `engine` or a
   related word but reduce the score of those that contain `private`
   because `private` is related to the negated word `privateer`.

7. When `,inbound` is followed with an URL, Babelia will return a list
   of documents that links to that URL.

8. When `,explore` is followed with an URL, Babelia will display a
   graphical interface to explore the Internet network of that page.

## Typofix

When for a given query, there is not hits, Babelia will try its best
to suggest typofixes, or suggest queries to speed a little the flow.

## Query suggestion

Babelia can also narrow your search query, and will suggest related
queries based on the content of the search engine to help in your
adventure.

Also, when Babelia consider that the query will take too much time, it
will still suggest related queries that will take an appropriate
amount of time.

## Entity recognition

When Babelia match one or more words to an entity found in the
knowledge base, it will display a box that describe what it
recognized.

## Search Pad

A search pad is semi-private journal of your search queries with their
results.  You can create a search pad using the menu on the top-right
corner.

When using th search pad, the input query `,note` possibly followed by
some text will return no results, but will allow you to save some
insight for your future self.
