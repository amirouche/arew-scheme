; Evaluation of this file yields an HTML document
; $Id: ssax-sourceforge.scm,v 1.6 2004/09/16 21:07:40 oleg Exp $

(define Content
'(html:begin
  (Header
   (title "XML and Scheme")
   (description "Representing, authoring, querying and transforming
markup data in Scheme; XML notation for a programming language")
   (Date-Revision-yyyymmdd "20040916")
   (Date-Creation-yyyymmdd "20010706")
   (keywords "XML, XML parsing, XML Infoset, XPath, XSLT, SAX, SXML, SXSLT, SXPath, Scheme, HTML composition, HTML authoring")
   (AuthorAddress "oleg-at-okmij.org")
   (long-title "S-exp-based XML parsing/query/conversion")
   )

  (body
   (navbar
     ("Home" "http://ssax.sourceforge.net")
     ("Docs" "#Documentation")
     ("Availability" "#Availability")
     ("CVS" "http://sourceforge.net/cvs/?group_id=30687")
     ("Summary" "http://sourceforge.net/projects/ssax/")
     ("Discussion" "http://sourceforge.net/mail/?group_id=30687")
     ("News" "http://sourceforge.net/news/?group_id=30687")
     ("Related" "#SXML-related"))

   (page-title)

   (a (@ (href "http://sourceforge.net")) " "
      (img (@ (src "http://sourceforge.net/sflogo.php?group_id=30687")
	      (width "88") (height "31") (border "0")
	      (alt "SourceForge Logo"))))

; Ref to Makoto Satoo

   (p "This project offers tools to inter-convert between an
angular-bracket and a more efficient S-expression-based notations for
markup documents, and to manipulate and query xML data in Scheme. The
main components of the project are SSAX, SXML, SXPath, and SXSLT.")

   (p
    "A SSAX functional XML parsing framework consists of a DOM/SXML
parser, a SAX parser, and a supporting library of lexing and parsing
procedures. The procedures in the package can be used separately to
tokenize or parse various pieces of XML documents. The framework
supports XML Namespaces, character, internal and external parsed
entities, attribute value normalization, processing
instructions and CDATA sections.  The package includes a
semi-validating " (em "SXML parser") ": a DOM-mode parser that is an
instantiation of a SAX parser (called SSAX).")

   (p "" (strong "SSAX") " is a full-featured, algorithmically optimal,
pure-functional parser, which can act as a stream processor. SSAX is
an efficient SAX parser that is " (em "easy to use") ". SSAX minimizes
the amount of application-specific state that has to be shared among
user-supplied event handlers. SSAX makes the maintenance of an
application-specific element stack unnecessary, which eliminates
several classes of common bugs. SSAX is written in a pure-functional
subset of Scheme. Therefore, the event handlers are referentially
transparent, which makes them easier for a programmer to write and to
reason about.  The more expressive, reliable and easier to use
application interface for the event-driven XML parsing is the outcome
of implementing the parsing engine as an enhanced tree fold
combinator, which fully captures the control pattern of the
depth-first tree traversal.")

  (p
    "" (strong "SXML") " is an abstract syntax tree of an XML
document. SXML is also a concrete representation of the XML Infoset in
the form of S-expressions.")

  (p
    "" (strong "SXSLT") " is a manipulation language for XML and
" (strong "SXPath") " is an XPath-conforming XML query language. Both
languages internally rely on SXML as a representation of the XML Infoset.")

    (ul
      (li (local-ref "Documentation"))
      (li (local-ref "Availability")
	(ul (li (local-ref "CVS"))
	    (li (local-ref "Distributions"))))
      (li (local-ref "SXML-related")
	(ul (li (local-ref "SXML"))))
      (li 
	(a (@ (href "http://lists.sourceforge.net/lists/listinfo/ssax-sxml"))
	  "SSAX-SXML Mailing list"))
      (li (a (@ (href "http://sourceforge.net/projects/ssax"))
	    "SSAX project summary page at SourceForge"))
      )

; Add the news section

   (Section 3 "Documentation" " and tutorials")
   (p "Main SSAX/SXML page:"
      (URL "http://pobox.com/~oleg/ftp/Scheme/xml.html"))

    (p "XML Matters: Investigating SXML and SSAX: "
       "Manipulating XML in the Scheme programming language" (br)
      "by David Mertz, Ph.D. " 
      "IBM developerWorks article"
      (URL "http://www-106.ibm.com/developerworks/library/x-matters31.html")
      (URL 
	"http://www-6.ibm.com/jp/developerworks/xml/040402/j_x-matters31.html")
      "[Japanese]")

   (p
    "Detailed introduction, motivation and real-life case-studies of
SSAX, SXML, SXPath and SXSLT." (br) "The paper and the complementary talk
presented at the International Lisp Conference 2002."
    (URL "http://pobox.com/~oleg/ftp/papers/SXs.pdf")
    (URL "http://pobox.com/~oleg/ftp/papers/SXs-talk.pdf")
    )

   (p
    "The derivation of the SSAX API and the comparison of SSAX with other
functional-style XML parsers and with the Expat" (br)
"A transcript of a presentation at PADL 2002, the 4th International
Symposium on Practical Aspects of Declarative Languages."
    (URL "http://pobox.com/~oleg/ftp/papers/XML-parsing-talk.ps.gz") (br)
    "The paper (Copyright of Springer-Verlag) is available at: "
    (URL "http://link.springer.de/link/service/series/0558/tocs/t2257.htm")
    (URL "http://pobox.com/~oleg/ftp/papers/XML-parsing.ps.gz"))

   (p
     "SXML tutorial (in Japanese)"
     (URL "http://homepage1.nifty.com/blankspace/scheme/nsx.html"))

   (p
     "SXML, SSAX, and SXML Transforms. Presentation, notes, and exercises" (br)
     "4th December 2002 and 8th January 2003 meetings of the Scheme UK group"
     (who "Noel Welsh and Matt Jadud")
     (URL "http://schematics.sourceforge.net/scheme-london/"))

    (p
      "Advanced SXLST tutorial"
      (URL "http://pobox.com/~oleg/ftp/Scheme/sxslt-advanced.scm") (br)
      "This file describes common patterns of SXSLT on an interesting
example. We demonstrate higher-order tags, pre-order and post-order
transformations, re-writing of SXML elements in regular and special
ways, context-sensitive applications of re-writing rules, and
reflection. Although the file is technically a Scheme source code, it is
actually a tutorial. Of 357 lines in the file, 241 are comments and 24
are just blank lines. August 2003.")
    (p 
      "SXSLT: Manipulation Language for XML"
      (URL "http://pobox.com/~oleg/ftp/papers/SXSLT-talk.pdf") (br)
      "A transcript of a presentation at PADL 2003. The talk introduces
SXSLT and compares it with XSLT.  Our experience and user comments
show that SXSLT is expressive and easy to use. We argue that this
outcome is a consequence of SXSLT providing right abstractions for XML
transformations, of being higher-order, declarative and extensible.")


   (Section 3 "SXML-related" " projects")

   (p
     "Sedna - a Native XML DBMS"
     (URL "http://www.modis.ispras.ru/Development/sedna.htm") (br)
     "Since version 0.2, Sedna provides the representation of query
results in SXML")

   (p
     "SXPath library: an implementation of XPath. The SXML query library."
     (br)
     "SXPointer: an SXPath-based implementation of XPointer"
     (who "Kirill Lisovsky")
     (URL "http://pair.com/lisovsky/query/"))

   (p
     "SXLink: An implementation of W3C XLink"
     (who "Dmitry Lizorkin")
     (URL "http://pair.com/lisovsky/download/contrib/xlink/"))

   (p
     "STX: a compiler for a subset of XSLT and an embedding of XSLT
into Scheme"
     (who "Kirill Lisovsky")
     (URL "http://pair.com/lisovsky/transform/stx/"))
    
    (p "DataGuides: A descriptive database schema for XML/SXML data"
     (who "Kirill Lisovsky")
      (URL "http://pair.com/lisovsky/xml/dg/"))

    (p
      "WebIt! - An XML Framework for Scheme"
      (who "Jim Bender")
      (URL "http://celtic.benderweb.net/webit/"))

   (p
     "HtmlPrag: a permissive HTML parser that emits SXML"
     (who "Neil W. Van Dyke")
     (URL "http://www.neilvandyke.org/htmlprag/"))

   (p
     "WebScraperHelper: simple generation of SXPath queries to extract 
data from (parsed) Web pages"
     (who "Neil W. Van Dyke")
     (URL "http://www.neilvandyke.org/webscraperhelper/"))

   (p
     "sxmlcnv: XML <-> SXML SmartDoc-friendly conversion application"
     (who "Kiyoka Nishiyama")
     (URL "http://www.netfort.gr.jp/~kiyoka/sxmlcnv/"))

   (Section 4 "SXML" " for static and dynamic websites")
   (p
     "The website of the MISA Technical University (one of the top ten
technical universities in the whole Russia) has been developed using
STX:"
     (URL "http://ir.misis.ru/english/about/general.htm"))
   (p
     "A Metcast server and its front-end access tools extensively rely on SXML"
     (URL "http://www.metnet.navy.mil/cgi-bin/oleg/get-advisories")
     (URL "http://www.metnet.navy.mil/Metcast/Code/get-advisories.scm"))
   (p "This page and all SXML-related pages are authored in SXML.")
    (p
      "For more detailed explanation of these projects, see the talk
at the International Lisp Conference.")



   (Section 3 "Availability")
   (p "The current released version of SSAX is 5.1.
The whole SSAX code is in public domain.")

   "SSAX has been tested on the following Scheme systems:"
   (br)
   "PLT Scheme, Bigloo, GambitC 3.0,
Chicken, Guile, SCM, MIT Scheme 7.5.2, Scheme48, SCSH, Gauche, SISC."


   (Section 3 "Distributions")
   (p "SSAX download site at SourceForge:"
      (URL "http://sf.net/project/showfiles.php?group_id=30687"))

   (p "Kirill Lisovsky's index of various SSAX distributions. Kirill
has put together many of those distributions, in particular, the ones
for PLT Scheme."
     (URL "http://pair.com/lisovsky/xml/ssax/"))

    (p "SSAX/SXML has been integrated into various Scheme systems
and, in some cases, become part of the distribution for those systems:")
    (ul
      (li "guile-lib, version 5.1"
	(who "Andy Wingo"))

      (li "Gauche/SXML, SSAX version 4.9+"
	(who "Shiro Kawai")
	(URL "http://cvs.sourceforge.net/viewcvs.py/gauche/Gauche/ext/sxml/"))

      (li "Bigloo SSAX-SXML library, version 4.9"
	(who "Kirill Lisovsky and Dmitry Lizorkin, maintained by
Dmitry Lizorkin")
	(URL "http://pair.com/lisovsky/download/sxml/bigloo/"))

      (li "SCSH and Scheme48, version 4.9"
	(who "Michael Sperber")
	(URL "http://www.scsh.net/resources/markup.html"))

      (li "Chicken ssax.egg, version 4.9.1"
	(who "Felix Winkelmann")
	(URL "http://www.call-with-current-continuation.org/eggs/ssax.html"))

      (li "SISC, an extensible Java based interpreter, SSAX version 4.9"
	(who "Noel Welsh")
	(URL "http://sisc.sourceforge.net/wiki/wakka.php?wakka=SSAX"))
      )


  (Section 3 "CVS" " Tree")
  (p (a (@ (href "http://sourceforge.net/cvs/?group_id=30687"))
	"The CVS Tree")
     " includes the complete SSAX/SXML code, some
documentation, validation tests, as well as several sample applications.")
  (p "You can "
     (a (@ (href "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ssax"))
	"browse the files in the CVS tree")
     " from any web browser.")


;    (dl

;     (dt (a (@ (href  "http://sourceforge.net/tracker/?group_id=30687"))
; 	   "Trackers"))
;     (dd "You can use a tracker to make a suggestion, to request a
; feature, or to report a problem.")

;     (dt (a (@ (href "http://sourceforge.net/forum/?group_id=30687"))
; 	   "Forums"))
;     (dd "You can browse, search, and post messages related to SSAX and SXML"
; 	(br) (n_))

;     (dt (a (@ (href "http://sourceforge.net/docman/?group_id=30687"))
; 	   "Doc Manager"))
;     (dt (a (@ (href "http://sourceforge.net/pm/?group_id=30687"))
; 	   "Task Manager"))
;     (dt (a (@ (href "http://sourceforge.net/survey/?group_id=30687"))
; 	   "Surveys"))
;     )

   (footer)

)))

;(pp Content)

;========================================================================
;			HTML generation

; IMPORT
; SXML-to-HTML-ext.scm and all of its imports


; Generating HTML

(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content
   (generic-web-rules Content 
     `((who *macro*
	 . ,(lambda (tag . elems)
	      `((n_) "[by " ,elems "]")))

		; A reference to an anchor in the present file
		; (local-ref target . title)
		; If title is given, generate a regular
		;	<a href="#target">title</a>
		; Otherwise, transform the content so that a
		; construct that may generate an anchor 'target' (such
		; as Section) is re-written to the
		; title SXML. All other constructs re-write to
		; nothing.
     (local-ref
      *preorder*
      . ,(lambda (tag target . title)
	   (let
	       ((title
		 (if (pair? title) title	; it is given explicitly
		     (pre-post-order Content
		       `((*text* . ,(lambda (trigger str) '()))
			 (*default*
			  . ,(lambda (tag . elems)
			       (let ((first-sign (signif-tail elems)))
				 (if first-sign
				     (let ((second-sign
					    (signif-tail (cdr first-sign))))
				       (assert (not second-sign))
				       (car first-sign))
				     '()))))
			 (Section
			  *preorder*
			  . ,(lambda (tag level key . elems)
			       (if (equal? key target)
				   (list key elems)
				   '()))))))))
	     (assert (pair? title) report: target)
	     (cerr "title: " title nl)
	     (post-order 
	      `(a (@ (href #\# ,target)) ,title)
	      universal-conversion-rules))))

       ; (navbar (title url) ...)
       (navbar
	*preorder*
	. ,(lambda (tag . elems)
	     (post-order
	       `(p (hr (@ (size 1) (noshade)))
		  (div (@ (align "center"))
		    ,(map
		     (lambda (title-url)
		       `((a (@ (href ,(cadr title-url))) ,(car title-url))
			 (n_) "|" (n_)))
		     elems))
		  (hr (@ (size 1) (noshade))) (br))
	       universal-conversion-rules)))

       )))))

(generate-HTML Content)
