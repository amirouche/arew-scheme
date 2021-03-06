; Evaluation of this file yields LaTeX data for an article
; $Id: SXs.scm,v 3.0 2004/02/19 22:08:49 oleg Exp $

(define Content
 '(html:begin
   (Header
    (title "(S)XML tools")
    (description "W3C-compliant XML processing in Scheme. Submitted to the International LISP conference.")
    (Date-Revision-yyyymmdd "20021031")
    (Date-Creation-yyyymmdd "20020718")
    (keywords "XML, SXML, SXPath, SXSLT, SXPointer, STX, Scheme")
    (AuthorAddress ("oleg@okmij.org" "lisovsky@acm.org"))
    (Author ("Oleg Kiselyov" "Kirill Lisovsky"))
    (Affiliation ("FNMOC" "MISA University"))
    (Revision "$Id: SXs.scm,v 3.0 2004/02/19 22:08:49 oleg Exp $")
    (long-title "XML, XPath, XSLT implementations as SXML,
SXPath, and SXSLT")
    (Links
     (start "index.html" (title "Scheme Hash"))
     (contents "../README.html")
     (prev "xml.html")
     (next "web.html")
     (top "index.html")
     (home "http://pobox.com/~oleg/ftp/")))

  (body
   (navbar)
   (page-title)
   (abstract
    "This paper describes S-expression-based implementations of W3C
XML Recommendations, the embedding of XML data and XML query and
manipulation tools into Scheme, and our extensive practical experience
in using these tools. The S-expression-based data format and query and
transformation tools are conformant with W3C Recommendations: XML,
XPath, XSLT, and XPointer. The core of our technique is SXML: an
abstract syntax tree of an XML document. SXML is also a concrete
representation of the XML Infoset in the form of S-expressions. SXML
fully supports XML Namespaces, processing instructions, parsed and
unparsed entities.  An SSAX parser and pretty-printing tools
convert between SXML and the angular-bracket-format of XML documents."
    (br)
    "All SXML query and manipulation tools are implemented in
Scheme. XPath expressions, XSLT patterns and XPointer notation are
translated into Scheme data structures and functions. SXML tree is a
data structure, but it can be directly evaluated as if it were an
expression. These features make XML data and XML processing a part of
Scheme and eliminate a lexical barrier for XML processing. A
combination of W3C conformance with seamless integration into a
programming language is a distinctive feature of our tools."
    (br)
    "We have used our approach in real-life commercial and government
applications for over two years.  We provide " (em "production") "
examples of a weather data dissemination system, XML transformation
systems, and web services."
    (keywords)
    )

   (TOC)

;--------------------------------------------------
   (Section 2 "Introduction")

   
   (p
    "In this paper, we describe S-expression-based implementations of
W3C XML Recommendations: XML, XPath, and XSLT. We have written these
tools out of practical necessity, and we have used them in real-life
projects for over two years. Our work of developing weather data web
services and supply-chain management systems requires advanced
manipulation of XML documents. For example, element selection
predicates may have to relate points and areas on the globe, to query
external data sources, and to evaluate complex business logic. Although
some of these algorithms could, in principle, be written in XSLT, the
corresponding code will be beyond comprehension. XSLT has never been
intended to be a general-purpose programming language " (cite "XSLT")
". On the other hand, we have to preserve an investment into XSLT
presentation stylesheets and XSLT skills. Therefore, we needed tools
that \"natively understand the data structures inherent in XML and
enable optimized algorithms for processing it\" " (cite "Bosworth")
". At the same time the tools must be conformant and compatible with
the W3C Recommendations. Ideally the tools should let us blend XPath
and XSLT on one hand, and the advanced XML processing on the other
hand.")
   (p
    "The goal of a seamless extension of the W3C XML processing model
cannot be easily achieved with Java: Java by nature is an imperative
language, whereas XSLT is not. We have reached our goal with
Scheme. The choice of Scheme arises from an observation that XML as a
datatype follows the nested containment model. Hierarchies of
containers that comprise text strings and other containers can be
naturally described by S-expressions.  S-expressions are easy to parse
into an efficient internal representation that is suitable for
transformation, manipulation, and evaluation.  These properties make
S-expressions suitable to represent abstract syntax of XML.  On the
other hand, S-expressions turned out to be convenient programming
notation (much to the surprise of their inventor). S-expressions that
represent XML data, SXML, can therefore be evaluated as if they were
code. Thus a real, mature programming language for manipulating XML
data structures looks exactly like the data structures themselves.")
   (p
    "Section 2 describes the XML data model and introduces abstract
syntax of XML. Section 3 discusses SXML query language SXPath which is
also an implementation of XPath. Section 4 introduces (S)XML
transformations. SXPointer is introduced in " (cite "SXPointer") ";
space constraints prevent us from providing more detailed
discussion. All examples in Sections 2-4 are borrowed from real-life,
production use of SXML and SXML-based technologies. Section 5
considers one such example in more detail. The final two sections
briefly mention other approaches to XML transformations and
conclude.")


;--------------------------------------------------
   (Section 2 "SXML")

   (p
    "XML has brought S-expressions back to the fore, albeit in a
veiled format and burdened with heavy syntax. Stripping off the syntax
-- that is, parsing an XML document -- yields its abstract syntax
tree: a hierarchy of containers comprised of text strings and other
containers. Salient features of this hierarchy are described by the
XML Infoset " (cite "XML-Infoset") ". A Document Object Model (DOM)
tree, produced by many XML parsers, is one concrete realization of the
XML Infoset. The Infoset can also be realized by S-expressions, which
are eminently suitable for representation of nested containers. SXML
Specification " (cite "SXML") " defines such a realization. The fact
that SXML represents \"parsed\" XML makes SXML concise and its
applications simple. On the other hand, the fact that SXML is concrete
lets us store and communicate SXML expressions, print them out and
even write them by hand.")
   (p
    "We must stress that SXML represents XML in every detail, including
namespaces, external entities, processing instructions. SXML is an
extensible format and permits additions of items that, while
preserving XML \"semantics\", can speed up processing: backpointers to
the parent or other ancestor elements, hashes of element ids,
etc. An XML document represents attributes, processing instructions,
namespace specifications and other meta-data differently from the
element markup. By contrast, SXML represents element content and
meta-data uniformly -- as tagged lists. This uniform treatment of
attributes and elements is argued by James Clark " (cite "RNG-Design")
" as a virtue.  The uniformity of the SXML representation for
elements, attributes, and processing instructions simplifies queries
and transformations.")
   (p
    "An XML document can be converted into SXML by any parser.  In
particular, an SSAX parser " (cite "SSAX-paper") " includes a dedicated
function " (code "SSAX:XML->SXML") " for this purpose.")
   (p
    "Figures " (xref "TAF-XML") " and " (xref "TAF-SXML") " show a
sample XML document and its representation in SXML. The XML document
is a 24-hour Terminal Aerodrome Forecast (TAF) for a Paris/Orly
airport. Such forecasts are issued by every airport and are distributed by
a network of weather services, e.g., Metcast " (cite "Metcast")
". Metcast is distinguished by being the first production service to
disseminate synoptic observations, forecasts and advisories in
XML. The particular markup format, OMF, is described in
" (cite "OMF") ". The document in Fig. " (xref "TAF-XML") " is an
actual reply of a Metcast server.")

   (figure "TAF-XML"
      "A sample TAF report in XML"

      (verbatim
       "<!DOCTYPE Forecasts SYSTEM \"OMF.dtd\">"
       "<Forecasts TStamp=\"1004046956\">"
       "<TAF TStamp='1004029200' LatLon='48.716, 2.383' BId='71490'"
       "     SName='LFPO, PARIS/ORLY'>"
       "<VALID TRange='1004032800, 1004065200'>251700Z 251803</VALID>"
       "<PERIOD TRange='1004032800, 1004065200'>"
       "   <PREVAILING>21007KT 9999 SCT025 BKN035</PREVAILING>"
       "     <VAR Title='BECMG 1820' TRange='1004032860, 1004040000'>"
       "          18005KT CAVOK</VAR>"
       "     <VAR Title='BECMG 0002' TRange='1004054400, 1004061600'>"
       "          6000 SCT035 SCT090 BKN250</VAR>"
       "</PERIOD></TAF></Forecasts>"
       ))


   (figure "TAF-SXML"
      "The sample TAF report in SXML"

      (verbatim
       "(*TOP* (Forecasts (@ (TStamp \"1004046956\"))"
       "    (TAF (@ (TStamp 1004029200) (LatLon \"48.716, 2.383\")"
       "            (BId \"71490\") (SName \"LFPO, PARIS/ORLY\"))"
       "      (VALID (@ (TRange \"1004032800, 1004065200\")) \"251700Z 251803\")"
       "      (PERIOD (@ (TRange \"1004032800, 1004065200\"))"
       "        (PREVAILING \"21007KT 9999 SCT025 BKN035\")"
       "        (VAR (@ (Title \"BECMG 1820\") (TRange \"1004032860, 1004040000\"))"
       "          \"18005KT CAVOK\")"
       "        (VAR (@ (Title \"BECMG 0002\") (TRange \"1004054400, 1004061600\"))"
       "          \"6000 SCT035 SCT090 BKN250\")))))"
       )
      )

   (p
    "Representation of XML Namespaces in SXML is demonstrated in
Figure " (xref "Example-NS") ". The XML snippet is actually taken from
the XML Namespaces Recommendation. It is very close to shipping
documents that we encountered in some of our projects. SXML treats
namespaces in a manner intended by the XML Namespaces
Recommendation. This subject is discussed in " (cite
"SXML") " in more detail.")

   (table 
    (@ 
     (caption "A sample XML document with XML namespaces, and its SXML form")
     (key "Example-NS") (table-type "figure") (align "center"))
    ;(tr (th "XML") (th "SXML"))
       
    (tr
     (td (@ (align "left"))
	    (verbatim
	     " "
	     " "
	     "<RESERVATION "
	     "  xmlns:HTML="
	     "   'http://www.w3.org/TR/REC-html40'>"
	     "<NAME HTML:CLASS=\"largeSansSerif\">"
	     "    Layman, A</NAME>"
	     "<SEAT CLASS='Y' "
	     "  HTML:CLASS=\"largeMonotype\">33B</SEAT>"
	     "<HTML:A HREF='/cgi-bin/ResStatus'>"
	     "    Check Status</HTML:A>"
	     "<DEPARTURE>1997-05-24T07:55:00+1"
	     "</DEPARTURE></RESERVATION>"))
     (td (@ (align "left"))
	 (verbatim
	     "(*TOP* (*NAMESPACES* "
	     "          (HTML \"http://www.w3.org/TR/REC-html40\"))"
	     "  (RESERVATION"
	     "    (NAME (@ (HTML:CLASS \"largeSansSerif\"))"
	     "      \"Layman, A\")"
	     "    (SEAT (@ (HTML:CLASS \"largeMonotype\")"
	     "             (CLASS \"Y\"))"
	     "       \"33B\")"
	     "    (HTML:A (@ (HREF \"/cgi-bin/ResStatus\"))"
	     "       \"Check Status\")"
	     "    (DEPARTURE \"1997-05-24T07:55:00+1\")))")))
	)
      

   
;--------------------------------------------------
   (Section 2 "Queries")

   (p
    "XPath " (cite "XPath") " is a basic query language of XML, which
is used to access an abstract XML data structure in XSLT, XPointer,
and XLink W3C Recommendations. Queries over SXML documents are
likewise expressed in SXPath. Just as SXML is closely related to XML,
so is SXPath to XPath. XPath addresses an abstract XPath data
structure; SXPath queries SXML, which is a concrete representation of
the XPath data structure. Both the abbreviated and full-form XPath
notations find their counterpart in SXPath. Just as XML can be
translated into SXML, XPath expressions can similarly be translated into
SXPath. This translation makes SXPath a compliant implementation of
the XPath Recommendation.")
   (p
    "Similarly to XPath, SXPath has several levels: lower-level predicates,
filters, selectors and combinators; and higher-level, abbreviated
SXPath notation. The latter has two formats: a " (em "list") " of location
path steps (\"native\" SXPath), or a character string of a XPath
expression (\"textual\" SXPath). The format of the latter string is
defined in the W3C XPath Recommendation " (cite "XPath") ". Native SXPath
expressions can be regarded a \"parsed\" form of XPath.
Low-level SXPath functions constitute a virtual machine into which
both kinds of higher-level expressions are compiled.")
   (p
    "For example, given a TAF SXML expression in Fig. " (xref
"TAF-SXML") ", we can determine the validity time ranges of the forecasts
by evaluating:")
   (verbatim
    "((sxpath '(// TAF VALID @ TRange *text*)) document)"
    )
   "using the native higher-level SXPath notation, or"
   (verbatim
    "((txpath \"//TAF/VALID/@TRange/text()\") document)"
    )
   "with the textual SXPath. The first argument of " (code "txpath") "
is a fully XPath-compliant location path.  Both functions " (code
"sxpath") " and " (code "txpath") " translate into a low-level SXPath
expression:"

   (verbatim
    "((node-join "
    "    (node-closure (node-typeof? 'TAF))"
    "    (select-kids (node-typeof? 'VALID))"
    "    (select-kids (node-typeof? '@))"
    "    (select-kids (node-typeof? 'TRange))"
    "    (select-kids (node-typeof? '*text*)))"
    " document)"
    )
   (p
    "Such SXPath expressions are used in a Scheme servlet that translates
OMF TAF documents sent by a Metcast server into a simple web page for
presentation on a PalmPilot-like handheld equipped with a wireless
receiver. The servlet is part of an aviation weather query tool 
" (cite "total-weather") " for operational users and small plane pilots.")
   (p
    "SXPath expressions can be more complex: for example, given a
booking record in Fig. " (xref "Example-NS") " we can select the names
of the passengers with confirmed reservations as follows:")

   (verbatim
     "((sxpath"
     "  `(// (RESERVATION ("
     "    ,(lambda (res-node)"
     "       (run-check-status"
     "            ; URL of the confirmation script"
     "        (car ((sxpath '(HTML:A @ HREF *text*)) res-node))"
     "        ((select-kids (node-typeof? '(NAME SEAT DEPARTURE)))"
     "         res-node)))"
     "    )) NAME *text*)) document)"
    )

   (p
    "We have seen that full-form or abbreviated SXPath expressions are
S-expressions. The full-form SXPath is actually a library of primitive
filters, selectors and combinators that can be combined in arbitrary
ways -- as well as combined with standard Scheme and user-defined
functions. Abbreviated SXPath expressions are S-expressions that may
include procedures and lambda-expressions, which thus extend the set
of selectors and predicates defined in XPath. For example, the
reservation query above relies on a custom predicate to retrieve a
dynamic web page and check the status. The predicate invokes high and
low-level SXPath functions to access particular fields of the
reservation record. The ability to use Scheme functions as SXPath
predicates, and to use SXPath selectors in Scheme functions makes
SXPath a truly extensible language. A user can compose SXML queries
following the XPath Recommendation -- and at the same time rely on the
full power of Scheme for custom selectors.")


;--------------------------------------------------
   (Section 2 "S-expressions and transformations")

   (p
    "The W3C XML transformation language XSLT " (cite "XSLT") "
defines XML transformations as converting a source (abstract XML
syntax) tree into a result tree. The conversion can be thought of as a
recursive traversal/mapping process: we visit tree nodes, locate the
corresponding node mapping handler in the transformation environment
and execute the handler. If we traverse the tree in pre-order the
handler is applied to the branch of the source tree. We can also
employ a post-order mode: once we enter a branch, we first traverse
its children and apply the handler to the transformed children. The
handler can be located by the name of an element node or by more
complex criteria. If an XML tree is realized as an SXML expression,
the described process is literally implemented by a function
pre-post-order whose complete code is part of the SSAX project " (cite
"SSAX") ". The function takes a source SXML tree and the
transformation environment and yields the result tree. The environment
is a list of associations of node names with the corresponding
transformers. Special bindings " (code "*text*") " and " (code
"*default*") " may be used in order to define catch-all transformation
rules for elements and atomic data. The transformation environment
passed to pre-post-order is essentially a transformation " (em
"stylesheet") ". It is also an S-expression. Thus the document to
transform and its transformation are expressed uniformly in the same
language.")
   (p
    "We should note that a transformation of an SXML tree in post-order is
equivalent to its evaluation as if it were a piece of Scheme
code. Pre-order transformations are similar to macro-expansions of the
tree -- again, as if it were a piece of Scheme code. The pre-order
traversal is built into XSLT.")
   (p
    "One example of SXML transformations is pretty-printing of SXML
into HTML, XML or " (LaTeX) ". Incidentally, this facility lets us
author a text in SXML and then convert it into a web page or a printed
document. When writing a text in SXML we can use higher-order tags (similar
to " (LaTeX) " macros) to make the markup more
logical. Compared to " (LaTeX) " macros, SXML markup is more
expressive: for example, the process of expansion of one SXML tag can
re-scan the entire SXML document with a different stylesheet. Such a
reflexive behavior is a necessity while generating hierarchical tables
of contents or resolving cross- and citation references.  The SXML
Specification is an example of such a higher-order markup. The
specification is written in SXML itself and relies on higher-order
tags to describe SXML grammar in a concise and abstract way. The
present paper is also written in SXML and then converted into PDF
through " (LaTeX) ".")
   (p
    "Another example of SXSLT is an XML transformation system STX
" (cite "STX") ", which, on the surface, is a processor for a
frequently-used subset of XSLT and therefore compatible with the W3C
XSLT Recommendation " (cite "XSLT") ". Behind the scene, STX
translates both the source document and the stylesheet into SXML, and
then literally applies one to the other. Therefore, STX permits
embedding of Scheme functions into XSLT templates (" (code
"scm:eval") ") to express complex business logic. More importantly, STX
allows an advanced user to write template rules as regular first-class
Scheme functions (" (code "scm:template") ") similarly to the SXML
transformers in the pre-post-order stylesheets described above.")
   (p
    "STX was originally designed as one of the development tools for
ECIS (Extranet Customer Information System) project in the IT department of
the Moscow branch of Cargill Enterprises Inc. Eventually it evolved
into a general-purpose XML transformation tool that provides the full
power of Scheme programming language for complex transformation and
business logic implementation and at the same time allows to reuse
existing XSLT skills and presentational stylesheets.")


;--------------------------------------------------
   (Section 2 "Detailed Example: Concise XML Authoring")

   (p
    "In this section, we elaborate a non-contrived and less-trivial
example of building XML documents from S-expressions.  It is
straightforward to convert " (code "<tag>data</tag>") " into " (code
"(tag \"data\")") " and vice versa. The SSAX parser and the SXML
manipulation tools can do that easily. However, exporting relational
sources into XML often runs into an impedance mismatch: XML by nature
is a hierarchical database. We will provide an example of generating
XML from S-expressions that involves denormalizations, table joins and
third-order tags. The S-expression format turns out to be not only
more understandable and insightful, but also four times shorter.")
   (p
    "The example is based on a real-life project of preparing a
submission of a synoptic markup format OMF " (cite "OMF") " for
U.S. Department of Defense's XML registry. The registry " (cite
"XML-DoDReg") " accepts submissions of XML formats as collections of
DTD/Schema documents, textual descriptions, sample code, etc. Every
submission package must include a Manifest.xml file, which describes
all containing documents as well as every markup element and its
attributes.")
   (p
    "Figure " (xref "Manifest-XML-snippets") " shows a representative
example of the manifest file. The snippets are taken from the actual
Manifest.xml " (cite "Manifest-XML") " and edited for brevity. They are
still hardly readable. The OMF submission contained 109
resources; therefore, preparing such a manifest manually was out of
the question. We will see shortly how that ugly document can be
represented in a concise and pleasing form.")

   (figure "Manifest-XML-snippets"
      "Manifest of an XML registry submission"
     (
      (p "Description of a sample XML document:")

      (verbatim
       "<AddTransaction><EffectiveDate>27 February 2001</EffectiveDate>"
       "<Definition>OMF Example: METAR/SYNOP/SPECI</Definition>"
       "<Namespace>MET</Namespace>"
       "<InformationResourceName>OMF-sample.xml</InformationResourceName>"
       "<InformationResourceVersion>OMF2.2</InformationResourceVersion>"
       "<InformationResourceTypeXMLSample>"
       "<InformationResourceLocation>OMF-sample.xml</InformationResourceLocation>"
       "<Relationships><DescribedBy><Namespace>MET</Namespace>"
       "<InformationResourceName>OMF-SYNOP.html</InformationResourceName>"
       "<InformationResourceVersion>OMF2.2</InformationResourceVersion>"
       "</DescribedBy></Relationships>"
       "</InformationResourceTypeXMLSample></AddTransaction>"
       )

      (p "Description of one XML element (element BTSC) within the submitted
collection:")

      (verbatim
       "<AddTransaction><EffectiveDate>12 April 2000</EffectiveDate>"
       "<Definition>an observation report on temperature, salinity and"
       "currents at one particular location on the ocean surface, or in"
       "subsurface layers</Definition><Namespace>MET</Namespace>"
       "<InformationResourceName>BTSC</InformationResourceName>"
       "<InformationResourceVersion>OMF4.1</InformationResourceVersion>"
       "<InformationResourceTypeXMLElement><DataTypeContainer><Contains><Namespace>MET</Namespace>"
       "<Contains><Namespace>MET</Namespace>"
       "<InformationResourceName>BTLEVELS</InformationResourceName>"
       "<InformationResourceVersion>OMF4.1</InformationResourceVersion>"
       "</Contains></DataTypeContainer>"
       "<Relationships><IsQualifiedByAttribute><Namespace>MET</Namespace>"
       "<InformationResourceName>TStamp</InformationResourceName>"
       "<InformationResourceVersion>OMF4.1</InformationResourceVersion>"
       "</IsQualifiedByAttribute>"
       "<IsQualifiedByAttribute><Namespace>MET</Namespace>"
       "<InformationResourceName>Depth</InformationResourceName>"
       "<InformationResourceVersion>OMF4.1</InformationResourceVersion>"
       "</IsQualifiedByAttribute>"
       "<DescribedBy><Namespace>MET</Namespace>"
       "<InformationResourceName>OMF-BATHY.html</InformationResourceName>"
       "<InformationResourceVersion>OMF1.4</InformationResourceVersion>"
       "</DescribedBy></Relationships>"
       "</InformationResourceTypeXMLElement></AddTransaction>"
       )


      (p "Description of an attribute, TStamp, which annotates a BTSC
element:")

      (verbatim
       "<AddTransaction><EffectiveDate>12 April 2000</EffectiveDate>"
       "<Definition>Time Stamp</Definition><Namespace>MET</Namespace>"
       "<InformationResourceName>TStamp</InformationResourceName>"
       "<InformationResourceVersion>OMF4.1</InformationResourceVersion>"
       "<InformationResourceTypeXMLAttribute><DataTypeInteger><IntegerLength>10</IntegerLength>"
       "<IntegerUnitMeasure>second</IntegerUnitMeasure>"
       "</DataTypeInteger>"
       "<Relationships><DescribedBy><Namespace>MET</Namespace>"
       "<InformationResourceName>OMF.html</InformationResourceName>"
       "<InformationResourceVersion>OMF2.2</InformationResourceVersion>"
       "</DescribedBy></Relationships>"
       "</InformationResourceTypeXMLAttribute></AddTransaction>"
       )
      ))

   (p
    "The manifest file is essentially a collection of resource
descriptions and of statements of resource relationships. It seemed
logical then to make such a structure explicit. Figure " (xref
"Manifest-SCM-snippets") " shows SXML code that corresponds to the
superset of the XML snippets. The SXML code is translated into
Manifest.xml by a function pre-post-order described in Section 4. The
SXML code and a transformation stylesheet for pre-post-order make up
the file Manifest.scm " (cite "Manifest-SCM") ".")

   (figure "Manifest-SCM-snippets"
      "The Manifest file in SXML"

      (verbatim
       "   (Resource \"OMF.html\""
       "             \"Weather Observation Definition Format (OMF) Document\" "
       "             \"10 March 2000\" \"2.2\")"
       "   (DescribeDoc \"OMF.html\")"
       ""
       "   (Resource \"OMF-SYNOP.html\""
       "             \"Surface Weather Reports from land and sea stations\" "
       "             \"12 April 2000\" \"2.2\")"
       "   (DescribeDoc \"OMF-SYNOP.html\")"
       ""
       "   (Resource \"BTSC\" \"an observation report on temperature, salinity"
       "and currents at one particular location on the ocean surface, or"
       "in subsurface layers\" \"12 April 2000\" \"4.1\")"
       "   (Resource \"BTID\" \"identification and position data, which"
       "constitute Section 1 of FM 62 - 64.\" \"12 April 2000\" \"4.1\")"
       "   (Resource \"BTLEVELS\" \"a sequence of BTLEVEL elements for each"
       "particular (sub)surface level described in a whole BTSC report\""
       "\"12 April 2000\" \"4.1\")"
       ""
       "   (XMLElement \"BTSC\" (DTContainer \"BTID\" \"BTCODE\" \"BTLEVELS\")"
       "               \"OMF-BATHY.html\""
       "               (Attlist \"TStamp\" \"LatLon\" \"BId\" \"SName\" \"Title\" \"Depth\"))"
       ""
       "   (XMLElement \"BTID\" (DTString 40) \"OMF-BATHY.html\""
       "               (Attlist \"DZ\" \"Rec\" \"WS\" \"Curr-s\" \"Curr-d\" \"AV-T\" \"AV-Sal\""
       "                        \"AV-Curr\" \"Sal\"))"
       ""
       "   (Resource \"TD\" \"The dew-point temperature\" \"12 April 2000\" \"4.1\")"
       "   (Resource \"TRange\" \"Time Interval\" \"12 April 2000\" \"4.1\")"
       "   (Resource \"TStamp\" \"Time Stamp\" \"12 April 2000\" \"4.1\")"
       ""
       "   (XMLAttr \"TD\" (DTFloat 6 2 \"deg C\") #f \"OMF-SYNOP.html\")"
       "   (XMLAttr \"TRange\" (DTString 30) #f \"OMF.html\")"
       "   (XMLAttr \"TStamp\" (DTInt 10 \"second\") #f \"OMF.html\")"
       )
      )

   (p
    "Let us consider two S-expressions from Fig. " (xref
"Manifest-SCM-snippets") " in more detail:")

   (verbatim
    "(Resource \"OMF-SYNOP.html\""
    "          \"Surface Weather Reports from land and sea stations\" "
    "          \"12 April 2000\" \"2.2\")"
    "(DescribeDoc \"OMF-SYNOP.html\")"
    )

   (p
    "If we take a naive view of SXML-to-XML transformations, we
might think that the corresponding Manifest.xml document " (cite
"Manifest-XML") " will include tags " (code "<Resource>") " and
" (code "<DescribeDoc>") ". In fact, the XML manifest document
contains neither. The S-expression " (code "Resource") " merely
declares the resource and serves as a container of its attributes: its
name, documentation string, modification date and 
version. During the SXML transformation, the S-expression translates
to nothing: the following is the handler for the " (code "Resource") "
tag in the SXML transformation stylesheet.")

   (verbatim
    "(Resource . ,(lambda (tag name title date version)"
       "    '()))               ; null expansion"
       )

   (p
    "Resources are described differently depending on their type. 
For example, " (code "(DescribeDoc \"OMF-SYNOP.html\")") " tells that " (code "OMF-SYNOP.html") " is a textual document. This S-expression will be transformed according to the following stylesheet rule:")

   (verbatim
    "(DescribeDoc    ; Describe a document resource"
    " . ,(lambda (tag name)"
    "      (generate-XML"
    "       `(AddTransaction"
    "         (Resource-descr ,name)"
    "         (InformationResourceTypeDocument"
    "          (InformationResourceLocation ,name)"
    "          )))))"
    )

   (p
    "The rule expands " (code "DescribeDoc") " into a set of SXML
elements required by the registry (e.g., " (code
"InformationResourceTypeDocument") "), which are distinguished by
their unwieldy names. The " (code "DescribeDoc") " handler invokes the
transformation function recursively, to effect the second pass. The
pass will transform " (code "(Resource-descr \"OMF-SYNOP.html\")") " according to the rule:")

   (verbatim
    "; Locate a named resource and expand into its full description."
    "(Resource-descr"
    " . ,(lambda (tag name)"
    "      (let-values* (((name title date version)"
    "                     (lookup-res name)))"
    "        (generate-XML"
    "         (list"
    "          (list 'EffectiveDate date)"
    "          (list 'Definition title)"
    "          '(Namespace \"MET\")"
    "          (list 'InformationResourceName name)"
    "          (list 'InformationResourceVersion \"OMF\" version))))))"
    )

   (p
    "The other tags and text strings will be handled by the default rules of the stylesheet:")

   (verbatim
    "(*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))"
    "(*text* . ,(lambda (trigger str) "
    "             (if (string? str) (string->goodXML str) str)))"
    )

   (p "The end result will be the difficult-to-read XML fragment in Fig. " (xref "Manifest-XML-snippets") ".")
   
   (p
    "The processing of " (code
"(Resource-descr \"OMF-SYNOP.html\")") " invokes a function " (code
"lookup-res") ", which queries the SXML Manifest for " (code
"(Resource \"OMF-SYNOP.html\" ...)") ". The fields of the found
S-expression are used to generate the proper resource description. In
database terms, the expansion of " (code "(DescribeDoc
\"OMF-SYNOP.html\")") " is a join of two tables.  The set of " (code
"(Resource ...)") " S-expressions in Manifest.scm represents one
table. S-expressions " (code "(DescribeDoc ...)") " make up the other
table. The resource's name (which is unique in a submission package)
is the primary key in both tables. The expansion of " (code
"DescribeDoc") " involved two re-writing steps, therefore, " (code
"DescribeDoc") " is a third-order tag.")
   (p
    "It seems that Manifest.scm describes the collection of OMF
resources in a more readable and understandable manner. It is instructive
to compare the file sizes of Manifest.scm and Manifest.xml:
Manifest.scm is 25831 bytes long, of which 9220 bytes are the
transformation stylesheet and the related code. The size of the file
Manifest.xml is 90377 bytes.")
   (p
    "The Manifest files in S-expression and XML formats are part of
the OMF submission into the DoD XML Registry. They can be retrieved
from the registry " (cite "Manifest-XML") ", " (cite "Manifest-SCM") ".")

;--------------------------------------------------
; ** Web services as a series of transformations

; SXML is advantageous not only for the representation of content but
; also for the denotation of control. S-expressions in form of agent
; communication languages have been successfully employed for control
; and manipulation. A (far heavier) form of S-expression -- SOAP -- is
; rapidly approaching the hype status now.

; It is not surprising that SXML can elegantly express desired actions
; as well as their arguments and results. What may seem unusual in our
; approach however is that the execution of actions -- the process of
; transforming a request into the result -- is a tree transformation,
; similar to the stylesheet transformations above. Therefore, the same
; set of tools, for example, the function post-order, could be used
; again.

; In this section, we present an example of such
; data-_and_-control transformations. The example is a three-tier Web
; service: a retrieval of Terminal Aerodrome Forecasts (TAFs)
;    [get-taf]. A client (a human being or a script) connects to a get-taf
; CGI script, the second tier. The client fills out a form, perhaps
; corrects noted errors, and finally submits it. The get-taf script
; executes a "remote procedure call" to a Metcast server, receives a TAF
; document similar to that on Fig. 1, and optionally converts it to a
; more presentable HTML page. SXML is being used on all stages: to
; specify the form, to represent user's input, to express the "remote
; procedure call", and to execute the XML-to-HTML transformation.

; The get-taf script indeed uses the same tree-transformation tool
; pre-post-order for several tasks. The query web form, Fig. xx, is
; coded in SXML. The form contains several high-level tags such as
;  (ffield-input-text time-mod-since int 5) which define a field of a
; form, specify its name and type constraints. The script transforms it
; into HTML, filling in form fields with default values. Later the
; script uses the same SXML form and the same pre-post-order function -- but
; a different stylesheet -- to convert the QUERY_STRING into bindings
; for form variables, validating the user input in the process. CGI
; processing is therefore a tree transformation, too. The set of form
; variables is finally re-written into an SXML-RPC request (called an
; MBL request).

;   {An additional advantage is common specification of parameters of the
; forms and their types; automatic type conversion and validation.}

; More details are available at [xxx]. Here we would like
; to mention only an example of a SXML-RPC/MBL request:

;  (webq
;   (bounding-box 90.0 -180 -90 180)
;   (modified-since 1000173287)
;   (st_constraint
;     (call_id "KMRY" "KSFO"))
;   (valid-at 1000143287)
;   (products (TAF (mime-type "text/plain"))))

; The get-taf Web form has a special button to show the generated MBL
; request for a particular user input. At the server side, the MBL
; request is further re-written into a SQL query. The result of the
; query is re-written into SXML (as in Sec 1.3), which is converted into
; XML and sent to the client. We could have sent the TAF report in its
; SXML form; however some clients insists on a syntax-heavy XML.


;   (verbatim
;  "(define Form"
;  "  `(html:begin \"Terminal Aerodrome Forecasts (TAFs)\""
;  "     (body"
;  "       (h1 \"Terminal Aerodrome Forecast Query\")"
;  "       (div (note-short))"
;  ""
;  "       (search-form \"Retrieving Terminal Aerodrome Forecasts (TAFs)\""
;  "         (table (@ (cellpadding \"2\") (cellspacing \"5\") (border \"0\"))"
;  "             (tr (th \"Call letters of a station or stations\")"
;  "                 (td (ffield-input-text call_id tokens 40)"
;  "                     (br)"
;  "                     (eg (code \"KMRY ksfo\") (n_)"
;  "                         \" Use whitespace to separate the entries\")))"
;  "             (tr (th \"Full or partial name of a station\")"
;  "                 (td (ffield-input-text name string 40)"
;  "                     (br)"
;  "                     (eg (code \"San Franci\"))))"
;  "             (tr (th \"Reported since\")"
;  "                 (td "
;  "                  (ffield-input-text time-mod-since int 5)"
;  "                  \" minutes ago\"))"
;  "             )"
;  "         (p \"You have to specify at least one search parameter.\")"
;  "         (p \"Metcast server to query: \""
;  "            (ffield-input-text mserver_url token 60 (maxlength 80)"
;  "                               (value ,METCAST-SERVER-URL)))"
;  "         (p (ffield-submit do-retrieve \"Retrieve\")"
;  "            (n_) (ffield-submit do-show \"Show MBL\"))"
;  "         )"
;  "       (p (hr))"
;  "       ))"
;  ")"
;  )

;   Fig. xx. The form from the get-taf script, edited for brevity


;--------------------------------------------------
   (Section 2 "Related work")

   (p
    "The fact that XML is S-expressions with significantly more syntax
was recognized fast " (cite "Wadler700") ". This realization was
followed by actions to recover the original beauty of S-expressions: 
" (cite "XT3D") ", " (cite "SXML-short-paper") ", " (cite "LAML") ". Even
some OpenSource projects chose to use S-expressions in their pristine
format " (cite "Slashdot") ".")
   (p
    "There are many ways to author, convert, or query XML documents,
relying either on template expansions, or embeddings of XML/HTML into
or around a host language. Examples include XSLT/XPath, ColdFusion,
Microsoft's Active Server Pages (ASP), Java Server Pages (JSP),
Hypertext Preprocessor (PHP). They all however introduce a particular
mini-language with a peculiar, non-XML-like syntax and ad-hoc
substitution semantics. The linguistic gap is wide. Representing an
XML/HTML document as code that upon execution will generate the
document is also a popular approach. CGI.pm in Perl, LAML in Scheme " (cite
"LAML") ", htmllib in Tcl, or Element Construction Set in Java
are only a few examples. Often such code even looks remotely like
S-expressions, albeit greatly more verbose and cumbersome, and
difficult to process reflexively.")


;--------------------------------------------------
   (Section 2 "Conclusions")
   (p
    "Immense popularity of XML is the testimony to the flexibility of
S-expressions. A great variety of data structures can be represented by
S-expressions; many operations can be regarded as transformations of
S-expressions. There is no reason, however, to hide S-expressions behind
layers of syntax. The most important benefits of this approach are the
use of a mature programming language for creation and processing
of semi-structured data and the disappearance of the linguistic gap
between XML data and processes to create and handle them.")
   (p
    "It does not seem likely, however, that the world would abandon
XML for S-expressions overnight. Therefore, we have to embrace the
existing formats and tools and be able to extend them. Our tools meet
this challenge. Internally we transform and evaluate S-expressions
and yet we accept and generate valid XML with Namespaces, entities and
other complexities. We can use many of the frequently-used XSLT
stylesheets as they are " (cite "STX") ", and can extend them through
embedded Scheme code and SXML-transformer-style templates. We can
write W3C-compliant XPath expressions along with their
S-expression-formatted counterparts, which we can extend with powerful
predicates in Scheme. We have used in practice such a transition path
from pure XML to more S-expression-based formats and tools. Due to a
unique combination of the expressive power of the Scheme language and
compatibility with W3C Recommendations, such an approach provides the
most sophisticated XML processing techniques along with a good
protection of investments in XML/XSLT solutions.")
   (p
    "The SXML parsing, query and transformation tools are released into public
domain as part of the SSAX project " (cite "SSAX") ".")


;--------------------------------------------------
   (References

    (bibitem "Bosworth" "Bosworth"
       "Adam Bosworth: A Programming Paradox. XML Magazine, February 2002."
       (URL 
	"http://www.fawcette.com/xmlmag/2002_02/magazine/departments/endtag/"))

     (bibitem "RNG-Design" "RNG-Design"
        "James Clark, The Design of RELAX NG. December 6, 2001."
        (URL "http://www.thaiopensource.com/relaxng/design.html"))

     (bibitem "XML-DoDReg" "XML-DoDReg"
	"DoD XML Registry"
	(URL "http://diides.ncr.disa.mil/xmlreg/user/index.cfm"))

     (bibitem "OMF" "OMF"
	"Oleg Kiselyov: Weather Observation Definition Format. March 8, 2000."
	(URL "http://zowie.metnet.navy.mil/~spawar/JMV-TNG/XML/OMF.html"))
                                                                  
     (bibitem "Metcast" "Metcast"
       "Oleg Kiselyov: Implementing Metcast in Scheme. Proceedings of the Workshop on Scheme and Functional Programming 2000. Montreal, 17 September 2000.")

     (bibitem "SXML-short-paper" "SXML-short-paper"
	"Oleg Kiselyov. XML and Scheme. An introduction to
SXML and SXPath;  illustration of SXPath expressiveness and comparison with
XPath. September 17, 2000."
	(URL "http://pobox.com/~oleg/ftp/Scheme/SXML-short-paper.html"))

;      (bibitem "get-taf" "get-taf"
;        "Oleg Kiselyov: Terminal Airodrome Forecast query. September 27, 2001." (br)
;        (URL "http://zowie.metnet.navy.mil/cgi-bin/oleg/get-taf"))

     (bibitem "total-weather" "total-weather"
       "Oleg Kiselyov: Aviation total weather and SIGMET advisory queries. June 12, 2000." (br)
       (URL "http://zowie.metnet.navy.mil/cgi-bin/oleg/get-sigmet-light")
       (URL "http://zowie.metnet.navy.mil/~dhuff/pqa/FNMOC.html"))

    (bibitem "SSAX-paper" "SSAX-paper"
       "Oleg Kiselyov. A better XML parser through functional programming. "
       "LNCS 2257, pp. 209-224. Springer-Verlag, January 2002.")

    (bibitem "SXML" "SXML"
       "Oleg Kiselyov. SXML Specification. Revision 2.1. March 1, 2002."
       (URL "http://pobox.com/~oleg/ftp/Scheme/SXML.html"))

    (bibitem "XT3D" "XT3D" 
      "Shriram Krishnamurthi, Gray, K.E. and Graunke, P.T.: "
      "Transformation-by-Example for XML. "
      "Practical Aspects of Declarative Languages, 2000.")

     (bibitem "STX" "STX"
	"Kirill Lisovsky. STX: Scheme-enabled Transformation of XML data. "
	(URL "http://pair.com/lisovsky/STX/"))

     (bibitem "SXPointer" "SXPointer"
	"Kirill Lisovsky. SXPath and SXPointer. "
	(URL "http://pair.com/lisovsky/sxml/sxpath/"))

    (bibitem "LAML" "LAML"
      "Kurt Normark. Programming World Wide Web Pages in Scheme. "
      "ACM SIGPLAN Notices, vol. 34, No. 12 - December 1999, pp. 37-46."
      (URL "http://www.cs.auc.dk/~normark/laml/"))


    (bibitem "Slashdot"  "Slashdot"
       "Black Parrot: Re:Scheme as an XML Translation Language. 
October 12, 2001. A comment in the thread \"Ask Kent M. Pitman About Lisp, Scheme And More\""
    (URL "http://slashdot.org/comments.pl?sid=22519&cid=2422286"))


    (bibitem "SSAX" "SSAX"
       "S-exp-based XML parsing/query/conversion. "
       (URL "http://ssax.sourceforge.net/"))

;     (bibitem "Wallace" "Wallace"
;        "Malcolm Wallace, and P. Runciman. "
;        "Haskell and XML: generic combinators or type-based translation?"
;        "Proc. the fourth ACM SIGPLAN international conference on Functional
;  programming, 1999, pp. 148 -159.")

    (bibitem "Wadler700" "Wadler700"
       "Philip Wadler: The Next 700 Markup Languages. Invited Talk,
Second Conference on Domain Specific Languages (DSL'99), Austin,
Texas, October 1999.")

    (bibitem "XML" "XML"
       "World Wide Web Consortium. Extensible Markup Language (XML)
1.0 (Second Edition). W3C Recommendation. October 6, 2000."
       (URL "http://www.w3.org/TR/REC-xml"))

     (bibitem "XML-Infoset" "XML-Infoset"
        "World Wide Web Consortium. XML Information Set.  W3C Recommendation. 24 October 2001."
        (URL "http://www.w3.org/TR/xml-infoset"))

     (bibitem "XML Namespaces" "XML Namespaces"
        "World Wide Web Consortium. Namespaces in XML. W3C Recommendation. January 14, 1999."
        (URL "http://www.w3.org/TR/REC-xml-names/"))

     (bibitem "XPath" "XPath"
        "World Wide Web Consortium. XML Path Language (XPath).
 Version 1.0. W3C Recommendation. November 16, 1999."
        (URL "http://www.w3.org/TR/xpath"))
    
    (bibitem "XSLT" "XSLT"
       "World Wide Web Consortium. XSL Transformations (XSLT). Version 1.0.
W3C Recommendation November 16, 1999."
       (URL "http://www.w3.org/TR/xslt"))

    (bibitem "Manifest-XML" "Manifest-XML"
	(URL "http://zowie.metnet.navy.mil/~spawar/JMV-TNG/XML/Manifest.xml")
	(URL "http://diides.ncr.disa.mil/xmlreg/package_docs/Public/ MET/OMF_package/997725059053/Manifest.xml"))

     (bibitem "Manifest-SCM" "Manifest-SCM"
	(URL
	 "http://zowie.metnet.navy.mil/~spawar/JMV-TNG/XML/Manifest.scm")
	(URL ("http://diides.ncr.disa.mil/xmlreg/" "package_docs/Public/ MET/OMF_package/997725059053/Manifest.scm")))

     )

  (footer)


)))

;------------------------------------------------------------------------
;			SXML->LaTeX conversion

; Given a string, check to make sure it does not contain characters
; such as '_' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate "escape sequences"

(define string->goodTeX
  (make-char-quotator
   '((#\# . "\\#") (#\$ . "\\$") (#\% . "\\%") (#\& . "\\&")
     (#\~ . "\\textasciitilde{}") (#\_ . "\\_") (#\^ . "\\^")
     (#\\ . "$\\backslash$") (#\{ . "\\{") (#\} . "\\}"))))


; Place the 'body' within the LaTeX environment named 'tex-env-name'
; options is a string or a list of strings that specify optional or
; mandatory parameters for the environment
; Return the list of fragments (suitable for SRV:send-reply)
(define (in-tex-env tex-env-name options body)
  (list "\\begin{" tex-env-name "}" options nl
	body
	"\\end{" tex-env-name "}" nl))


(define (generate-TEX Content)

 (SRV:send-reply
  (pre-post-order Content
   `(
			; General conversion rules
     (@
      ((*default*       ; local override for attributes
	. ,(lambda (attr-key . value) (cons attr-key value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems)
		       (cerr "Tag: " tag " ignored" nl)
		       '()))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodTeX str) str)))
     (n_		; a non-breaking space
      . ,(lambda (tag . elems)
	   (list "~" elems)))

     (html:begin
      . ,(lambda (tag . elems)
	   (list
	    "% SXML->TEX created this file" nl
	    "\\documentclass[10pt]{llncs}" nl
	    "\\usepackage[T1]{fontenc}" nl
	    "\\usepackage[latin1]{inputenc}" nl
	    ;"\\usepackage{times}" nl
	    ;"\\pagestyle{empty}" nl
	    "\\makeatletter" nl
	    "\\makeatother" nl
	    "\\sloppy" nl
	    "\\newcommand{\\minitab}[2][l]{\\begin{tabular}{#1}#2\\end{tabular}}" nl
	    "\\setlength\\topmargin{-10pt}" nl
	    "\\setlength\\textheight{53\\baselineskip}" nl
	    nl
	    elems
	    )))

     (Header		; It is just the placeholder for the meta-information
      *preorder*
      . ,(lambda (tag . headers) '()))

     (body
      . ,(lambda (tag . elems)
	   (in-tex-env "document" '() elems)))

     (page-title	
      . ,(lambda (tag) 
	   (let* ((header-parms (find-Header Content))
		  (title (lookup-def 'long-title header-parms #f))
		  (authors (car (lookup-def 'Author header-parms #f)))
		  (affiliations (car 
				 (lookup-def 'Affiliation header-parms #f)))
		  (emails (car 
			   (lookup-def 'AuthorAddress header-parms #f))))
	     (if (pair? authors)
		 ; several authors
		 (list "\\title{" title "}" nl
		       "\\author{"
		       (let loop ((authors authors) (count 1))
			 (if (null? authors) '()
			     (list (and (> count 1) " \\and ")
				   (car authors) "\\inst{" count "}"
				   (loop (cdr authors) (++ count)))))
		       "}" nl
		       "\\institute{"
 		       (list-intersperse 
 			(map
 			 (lambda (affiliation email)
 			   (list affiliation "\\" nl "\\email{" email "} "))
 			 affiliations emails)
 			" \\and ")
		       "}" nl
;		       "\\author{"
; 		       (list-intersperse 
; 			(map
; 			 (lambda (author affiliation email)
; 			   (list author ", " affiliation ", " email))
; 			 authors affiliations emails)
; 			"\\\\")
; 		       "}" nl
		 "\\maketitle" nl)
		 (list "\\title{" title "}" nl
		       "\\author{" authors 
		       "\\\\" affiliations
		       "\\\\" emails
		       "}" nl
		       "\\maketitle" nl)))))

     ; Standard typography
     (em
      . ,(lambda (tag . elems)
	   (list "\\emph{" elems "}")))

     (p
      . ,(lambda (tag . elems)
	   (list elems nl nl)))

     (div
      . ,(lambda (tag . elems)
	   (list elems nl nl)))

     (br
      . ,(lambda (tag)
	   (list "\\\\ ")))

     (code
      . ,(lambda (tag . elems)
	   ;(in-tex-env "ttfamily" '() elems) ;
	   (list "\\texttt{" elems "}")
	   ))

     (cite
      . ,(lambda (tag key)
	   (list "\\cite{" key "}")))

     (xref
      . ,(lambda (tag key)
	   (list "\\ref{" key "}")))

     (ul			; Unnumbered lists
      . ,(lambda (tag . elems)
	   (in-tex-env "itemize" '() elems)))

     (ol			; Numbered lists
      . ,(lambda (tag . elems)
	   (in-tex-env "enumerate" '() elems)))

     (li
      . ,(lambda (tag . elems)
	   (list "\\item " elems nl)))

     (dl			; Definition list
				; deal with the sequences of dt/dd in CPS

      ; dl and dt are translated to procedures that take one argument:
      ; previously set label: list of fragments or #f if none
      ; The procedure returns a pair: (new-label . generate-fragments)
      ; Initially, label is #f

      ((dt			; The item title
	. ,(lambda (tag . elems)
	    (lambda (label)
	      (cons elems	; elems become the new label
		    (if label	; the label was set: we've seen dt without dd
			(list "\\item [" label "]" nl) ; empty body
			'())))))
       (dd			; The item body
	. ,(lambda (tag . elems)
	    (lambda (label)
	      (cons #f		; consume the existing label
		    (list "\\item [" (or label "") "] " elems nl)))))
       )
      . ,(lambda (tag . procs)	; execute provs generated by dt/dd
	  (let loop ((procs procs) (label #f) (accum '()))
	    (if (null? procs) (in-tex-env "description" '() (reverse accum))
		(let ((result ((car procs) label)))
		  (loop (cdr procs) (car result) (cons (cdr result) accum))))))
      )
	
     (URL 
      . ,(lambda (tag url) (list " \\texttt{" url "} ")))

     (LaTeX
      . ,(lambda (tag) "\\LaTeX "))

     (blockquote
      . ,(lambda (tag . elems)
	   (in-tex-env "quote" '() elems)))

     (Section	; (Section level "content ...")
      . ,(lambda (tag level head-word . elems)
	   (list #\\
		 (case level
		   ((1 2) "section")
		   ((3) "subsection")
		   ((4) "subsubsection")
		   (else (error "unsupported section level: " level)))
		 "{" head-word elems "}" nl)))

     (abstract			; The abstract of the document
      ((Revision
	. ,(lambda (tag)	; Find the Header in the Content
				; and create the revision record
	     (let ((header-parms (find-Header Content)))
	       (list "The present article specifies revision "
		     (lookup-def 'Revision header-parms #f) " of SXML. "))))
       (prod-note
	. ,(lambda (tag . master-url)
	     (list
	      "\\\\The paper is written in SXML itself. The present paper is the result of translating that SXML code into \\LaTeX, using an appropriate \"stylesheet\". A different stylesheet converted the specification to HTML. The master file, the stylesheets and the corresponding HTML and \\LaTeX documents are available at "  master-url ".")))
       (keywords
	. ,(lambda (tag)
	     (let ((header-parms (find-Header Content)))
	       (list "\\\\Keywords: " 
		     (lookup-def 'keywords header-parms #f) "."))))

       )
      . ,(lambda (tag . abstract-body)
	   (in-tex-env "abstract" '() abstract-body))
      )

     (Acknowledgment
      . ,(lambda (tag title)
	   (list "\\subsection*{" title "}" nl)))

;      (References	; (References bibitem ...)
;       *preorder*	; (bibitem label key . text)
;       . ,(lambda (tag . bib-items)
; 	   (let
; 	       ((widest-label ; first search for the widest label
; 			      ; among the bibitems
; 		 (let loop ((items bib-items) (widest ""))
; 		   (if (null? items) widest
; 		       (loop (cdr items)
; 			     (let ((label (cadar items)))
; 			       (if (> (string-length label)
; 				      (string-length widest))
; 				   label widest))))))
; 		(processed-items
; 		 (map
; 		  (lambda (bib-item)
; 		    (apply
; 		     (lambda (tag label key . text)
; 		       (list "\\bibitem[" label "]{" key "} " text nl))
; 		     bib-item))
; 		  bib-items)))
; 	     (in-tex-env "thebibliography" (list "{" "" "}")
; 			 processed-items))))

      (References	; (References bibitem ...)
       ((bibitem
	 . ,(lambda (tag label key . text)
	      (list "\\bibitem{" key "} " text nl)))
	)
       . ,(lambda (tag . bib-items)
	    (in-tex-env "thebibliography" "{10}"
			bib-items)))

     (verbatim	; set off pieces of code: one or several lines
      ((*text* . ; Different quotation rules apply within a verbatim block
	       ,(let ((string->goodTeX-in-verbatim
		      (make-char-quotator
		       '( (#\^ . "\\^")
			 ))))
		  (lambda (trigger str) 
		    (if (string? str) (string->goodTeX-in-verbatim str) str)))
	       ))
      . ,(lambda (tag . lines)
	   (in-tex-env "verbatim" '()
		       (map (lambda (line) (list "     " line nl))
			    lines))))

     (table
		; verbatim mode does not work in tabular <deep sigh> ...
		; we have to emulate
      ((verbatim	
	((*text* . ; Different quotation rules apply within a "verbatim" block
	       ,(let ((string->goodTeX-in-verbatim
		      (make-char-quotator
		       '((#\space "~")	; All spaces are "hard"
			 (#\# . "\\#") (#\$ . "\\$") (#\% . "\\%")
			 (#\& . "\\&") (#\~ . "\\textasciitilde{}")
			 (#\_ . "\\_") (#\^ . "\\^")
			 (#\\ . "$\\backslash$") (#\{ . "\\{")
			 (#\} . "\\}")))))
		  (lambda (trigger str) 
		    (if (string? str) (string->goodTeX-in-verbatim str) str)))
	       ))
	. ,(lambda (tag . lines)	
	     (map (lambda (line) (list "\\texttt{\\small " line "}\\\\"))
		  lines)))
       (tr		; elems ::= [(@ attrib ...)] td ...
			; we disregard all attributes of a row
			; The result is (td ...)
	. ,(lambda (tag . elems)
	     (if (and (pair? elems) (pair? (car elems))
		      (eq? '@ (caar elems)))
		 (cdr elems)
		 elems)))

       (td		; elems ::= [(@ attrib ...)] body ...
			; we're only interested in (align "alignment") attr
			; and the (colspan "number") attr
			; The result is ("alignment" colspan body ...)
			; where "alignment" will be #\l, #\c, #\r
			; (#\l if not given); colspan is the integer
	. ,(lambda (tag . elems)
	     (define (get-alignment attrs)
	       (cond
		((assq 'align attrs) =>
		 (lambda (attr)
		   ;(cerr "align attr: " attr nl)
		   (cond
		    ((string-ci=? (cadr attr) "left") #\l)
		    ((string-ci=? (cadr attr) "right") #\r)
		    ((string-ci=? (cadr attr) "center") #\c)
		    (else (error "wrong alignment attribute: " attr)))))
		(else #\l)))
	     (define (get-colspan attrs)
	       (cond
		((assq 'colspan attrs) =>
		 (lambda (attr)
		   (let ((val (string->number (cadr attr))))
		     (assert val)
		     val)))
		(else 1)))
	     (if (and (pair? elems) (pair? (car elems))
		      (eq? '@ (caar elems)))
		 (cons (get-alignment (cadar elems))
		   (cons (get-colspan (cadar elems))
			 (cdr elems)))
		 (cons (get-alignment '())
		   (cons (get-colspan '())
			elems)))))

       (th		; elems ::= [(@ attrib ...)] body ...
			; we're only interested in (align "alignment") attr
			; and the (colspan "number") attr
			; The result is ("alignment" colspan body ...)
			; where "alignment" will be #\l, #\c, #\r
			; (#\c if not given); colspan is the integer
	. ,(lambda (tag . elems)
	     (define (get-alignment attrs)
	       (cond
		((assq 'align attrs) =>
		 (lambda (attr)
		   ;(cerr "align attr: " attr nl)
		   (cond
		    ((string-ci=? (cadr attr) "left") #\l)
		    ((string-ci=? (cadr attr) "right") #\r)
		    ((string-ci=? (cadr attr) "center") #\c)
		    (else (error "wrong alignment attribute: " attr)))))
		(else #\c)))
	     (define (get-colspan attrs)
	       (cond
		((assq 'colspan attrs) =>
		 (lambda (attr)
		   (let ((val (string->number (cadr attr))))
		     (assert val)
		     val)))
		(else 1)))
	     (if (and (pair? elems) (pair? (car elems))
		      (eq? '@ (caar elems)))
		 (cons (get-alignment (cadar elems))
		   (cons (get-colspan (cadar elems))
			 (cdr elems)))
		 (cons (get-alignment '())
		   (cons (get-colspan '())
			elems)))))
       )
			; (table [(@ attrib ...)] tr ...
      . ,(lambda (tag row . rows)
	   (let-values* 
	    (((attrs rows)
	      (if (and (pair? row) (eq? '@ (car row)))
		  (values (cadr row) rows)
		  (values '() (cons row rows))))
	     (border?
	      (cond
	       ((assq 'border attrs) => 
		(lambda (border-attr) (not (equal? "0" (cadr border-attr)))))
	       (else #f)))
	     ((caption label table-type table-alignment)
	      (apply values
		     (map (lambda (name)
			    (cond
			     ((assq name attrs) => cadr)
			     (else #f)))
			  '(caption key table-type align))))
	     (dummy (assert (pair? rows))) ; at least one row must be given
	     (ncols (length (car rows)))
	     (tex-cols
	      (let ((col-codes
		     (map (lambda (_) (if border? "l|" "l")) (car rows))))
		(if border?
		    (apply string-append 
			   (cons "|" col-codes))
		    (apply string-append col-codes))))
	     )
	    (list
	     (in-tex-env (or table-type "table") "[ht]"
	      (list
	       (and (equal? table-alignment "center")
		    "\\centering")
	       (in-tex-env "tabular"
			   (list "{" ; "@{\\extracolsep{-25pt}}" 
				 tex-cols "}")
		(list (and border? "\\hline\n")
		  (map
		   (lambda (row)
		     (list
		      (list-intersperse
		       (map
			(lambda (col)
			  (apply
			   (lambda (alignment span . data)
			     (list "\\multicolumn{" span "}{" alignment "}{"
				   "\\minitab[" alignment "]{"
				   data "}}")
			     )
			   col))
			row)
		       " & ")
		      "\\\\" (and border? "\\hline") nl))
		   rows)
		  nl))
	       (and caption (list "\\caption{" 
				  (and label 
				       (list "\\label{" label "}"))
				  caption "}"))
	       )))
	    )))

     ; Floating figure
     (figure
      . ,(lambda (tag refkey caption . body)
	   (in-tex-env "figure" '()
		       (list body
		       "\\caption{" (and refkey 
				       (list "\\label{" refkey "}"))
		       caption "}")
		       )))
		       
     ; Fancy characters
     (Longrightarrow
      . ,(lambda (tag)
	   "$\\Longrightarrow$"))

     ; Grammatical terms

     (production
      . ,(lambda (tag number lhs rhs . comment)
	   (list "{[}" number "{]} & \\texttt{" lhs "} & "
		 " $::=$ &"
		 " \\texttt{"
		 (if (pair? rhs)
		     (list-intersperse rhs " ")
		     rsh)
		 " } " comment)))

     (productions
      . ,(lambda (tag . prods)
	   (list
	    (in-tex-env "tabular"
			'("{"
			  "r" "r" "c" "p{2.8in}" "}")
			(map (lambda (item)
			       (list item "\\\\" nl))
			    prods))
	    "\\\\" nl)))
     )))
 )

(generate-TEX Content)

