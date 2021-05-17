# Ruse Scheme

**experimental**

## Babelia

Babelia is a privacy friendly, decentralized, open source, and
accessible search engine. Search has been an essential part of
knowledge acquisition from the dawn of time, whether it is antique
lexicographically ordered filing cabinets or nowadays computer-based
wonders such as Google or Bing. From casual search to help achieve
common tasks such as cooking, keeping up with the news, a regular dose
of cat memes or professional search such as science research. Search
is, and will remain, an essential daily-use tool, and stires human
progress forward.

Babelia aims to replace the use of privateer search engines with a
search engine that is open, hence under the control of the
commons. Babelia wants to be an easy to install, easy to use, easy to
maintain, no-code, personal search engine that can scale to billions
of documents, beyond a terabyte of text data, for under €100 a month
per Babelia instance.

## Software Bill of Materials

Backend:

- Chez Scheme (backend language)
- FoundationDB (database)
- SQLite LSM extension (database)
- libtls (bsd project, or curl)
- zstandard (facebook's compression library)
- blake3 (?)
- argon2 (password hashing)

Frontend:

- Ruse Scheme (frontend language)
- preactjs (frontend view library)
- Bootstrap (frontend styles)

Also:

- Debian (GNU/Linux distribution)
- nginx (HTTP proxy)
- Python (tooling)
- CoCrawler

## License

See `LICENSE` file.

## Acknowledgements

[![Logo NLnet: abstract logo of four people seen from above](https://nlnet.nl/logo/banner.png)](https://NLnet.nl)

[![Logo NGI Zero: letterlogo shaped like a tag](https://nlnet.nl/image/logos/NGI0_tag.png)](https://NLnet.nl/NGI0)

This project was funded through the [NGI0
Discovery](https://nlnet.nl/discovery) Fund, a fund established by
NLnet with financial support from the European Commission's [Next
Generation Internet](https://ngi.eu) programme, under the aegis of DG
Communications Networks, Content and Technology under grant agreement
No 825322.
