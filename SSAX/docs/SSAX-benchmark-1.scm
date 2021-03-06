; Evaluation of this file yields an HTML document
; $Id: SSAX-benchmark-1.scm,v 3.0 2004/02/19 22:08:48 oleg Exp $

(define Content
'(html:begin
  (Header
   (title "SSAX vs. Expat")
   (description "Comparison and benchmarking of XML parsers: SSAX vs. Expat")
   (Date-Revision-yyyymmdd "20030509")
   (Date-Creation-yyyymmdd "20011202")
   (keywords "XML parsing, Expat, SSAX, XML benchmark")
   (AuthorAddress "oleg-at-okmij.org")
   (long-title "Comparing SSAX and Expat XML parsers in features and performance")
   (Links
    (start "index.html" (title "Scheme Hash"))
    (contents "xml.html")
    (prev "xml.html")
    (next "parsing.html")
    (top "index.html")
    (home "http://pobox.com/~oleg/ftp/")))

  (body
   (navbar)
   (page-title)

   (TOC)

   (p "We present the results of a benchmark that compares the
performance of C and Scheme XML parsers. The benchmark gives an
insight into the performance of Scheme, compared to the best C
code. We also discuss differences between SSAX and Expat in terms
of features and the user interface.")

   (p "Originally this page was a compilation of two articles posted
on a newsgroup comp.lang.scheme On Nov 1 and 5, 2001, extended with
answers to common comments. This page reports the latest benchmark
results for the current, as of this writing, version of SSAX.")


   (Section 2 "Introduction to the benchmark")
	
   (p "The benchmark is \"untagging\" of an XML document realizing a full
binary tree, such as")
   (verbatim
 "<node><node><node><node><leaf>0</leaf><leaf>1</leaf></node>"
 "                  <node><leaf>2</leaf><leaf>3</leaf></node></node>"
 "            <node><node><leaf>4</leaf><leaf>5</leaf></node>"
 "                  <node><leaf>6</leaf><leaf>7</leaf></node></node></node>"
 "      <node><node><node><leaf>8</leaf><leaf>9</leaf></node>"
 "                  <node><leaf>0</leaf><leaf>1</leaf></node></node>"
 "            <node><node><leaf>2</leaf><leaf>3</leaf></node>"
 "                  <node><leaf>4</leaf><leaf>5</leaf></node></node>"
 "</node></node>")
   
   "The content of leaf nodes is a single-digit string. In loftier terms,
we compute a " (code "string-value") " of the document root."
	
   (Section 2 "Expat and SSAX")

   (p "To make the comparison meaningful, we need to discuss input models of
Expat and SSAX.")
   (p
    "Expat is " (em "the") " fastest XML parser " (cite
"Expat-tutorial") ". It is written in C by James Clark. It is a SAX
parser. An application that uses Expat must open an XML file or stream
and read its blocks into memory. The application should pass these
blocks to Expat, indicating the block size and if it is the last block
of the document. An application can potentially load the whole
document into memory and pass this single block to Expat. The parser
is specifically optimized for such a scenario, because Expat uses
shared substrings as much as possible. The first benchmark
application, " (code "string-value.c") ", reads the whole document
into memory, passes it to Expat and asks the parser to compute the
string value. It takes 0.05 sec user time for " (code
"string-value.c") " to handle an XML document that represents a full
binary tree of depth 15 (32768 leaf nodes, total size 884,723
bytes). This is indeed fast.")
   (p
    "The Expat input mode assumes that a calling application must know
when the document ends. Otherwise, an application cannot read the
document from a stream, in whole or in blocks. When the input document
is read from a regular file, it is trivial to find out when we are
finished reading. The OS will tell us. It is not so trivial to
determine the end of an XML document if it is wrapped in MIME or other
envelopes.  Furthermore, if we take a document from a (tcp) pipe, it
may be impossible to tell offhand when to stop reading. Furthermore,
if we unwittingly try to read one extra character, we become blocked
and possibly deadlocked.")
   (p
    "SSAX is also a SAX parser " (cite "SSAX") ". It is written completely in
Scheme. SSAX uses a different input model. Rather than relying on an
application to feed data to it, SSAX reads characters from a given
input port itself.  SSAX reads ahead by no more than one character,
and only when the parser is positive the character to read ahead must be
available. SSAX does not need to be told when the document is
ended. On the contrary, SSAX will tell you when it has finished
parsing a root (or other) element. SSAX therefore is safe to be used
on pipes, to process several documents in a file, or to handle
selected parts of a document.")
   (p
    "Therefore, to meaningfully compare SSAX with Expat, we need a
different benchmark application: " (code "string-value-by-one.c") " .
It first loads an XML document into a memory buffer. It then passes
the content of that buffer one character at a time to Expat. It takes
0.34 user seconds for " (code "string-value-by-one.c") " to handle the
same XML document as above (full binary tree of depth 15).")
   (p
    "A SSAX benchmark " (code "string-value-ssax.scm") " is fully
equivalent to " (code "string-value-by-one.c") ". This Scheme code
also loads an XML document, opens the memory buffer as a string
port and passes the port to SSAX. It takes 0.32 user seconds to handle
the same document.")

   (p "A benchmark " (code "string-value-ssax-ss.scm") " is also
Scheme code: it is a version of " (code "string-value-ssax") " that
relies on a custom function " (code "substring->symbol") ". The latter
avoids copying a substring if the symbol has already been seen. The
files substring-symbol.c, read-NCName-ss.scm and
string-value-ssax-ss.scm in " (cite "SSAX-benchmarks") " give more
details.")

   (p "The complete code for the benchmark is part of the SSAX project at SourceForge " (cite "SSAX-benchmarks") ".")

   (Section 2 "Summary of the performance benchmark")

   ; (table (@ (align "center") (cellpadding "1") (cellspacing "3"))
;     (tr
;      (th "string-value.c:") (th "XML-tree-depth-15")  (th "XML-tree-depth-16"))
;     (tr
;      (td "user time, s") (td "0.105") (td "0.213"))
;     (tr
;      (td "system time, s") (td "0.016") (td "0.022"))
;     (tr
;      (td "page reclaims") (td "241") (td "483"))
;     (tr (td (@ (colspan  3)) (n_)))
;     (tr
;      (th "string-value-by-one.c:") (th "XML-tree-depth-15") (th "XML-tree-depth-16"))
;     (tr
;      (td "user time, s") (td "0.747") (td "1.494"))
;     (tr
;      (td "system time, s") (td "0.014") (td "0.012"))
;     (tr
;      (td "page reclaims") (td "25") (td "49"))
;     (tr (td (@ (colspan  3)) (n_)))
;     (tr
;      (th "string-value-ssax.scm:") (th "XML-tree-depth-15") (th "XML-tree-depth-16"))
;     (tr
;      (td "user time, s") (td "1.092") (td "2.170"))
;     (tr
;      (td "system time, s") (td "0.024") (td "0.095"))
;     (tr
;      (td "page reclaims") (td "842") (td "2353"))
;     (tr (td (@ (colspan  3)) (n_)))
;     (tr
;      (td "File size, bytes") (td "884,723") (td "1,769,459"))
;     )

 (benchmark-table
   (files
     (b15 "bench1-file15.xml" 884723)
     (b16 "bench1-file16.xml" 1769459)
     (b17 "bench1-file17.xml" 3538931)
     (b18 "bench1-file18.xml" 7077875)
     (b19 "bench1-file19.xml" 14155763)
     (b20 "bench1-file20.xml" 28311539)
     )
   (legend "user time, s" "number of page reclaims")
   (result "string-value"
     (b15 0.05 241)
     (b16 0.09 483)
     (b17 0.18 963)
     (b18 0.36 1926)
     (b19 0.73 3850)
     (b20 1.43 7698))

   (result ("string-value" (br) "-by-one")
     (b15 0.34 25)
     (b16 0.68 49)
     (b17 1.38 97)
     (b18 2.74 193)
     (b19 5.52 385)
     (b20 11.04 770))

   (result ("string-value" (br) "-ssax")
     (b15 0.32 842)
     (b16 0.65 2353)
     (b17 1.31 5311)
     (b18 2.58 10283)
     (b19 5.52 16381)
     (b20 11.17 26513))

   (result ("string-value" (br) "-ssax-ss")
     (b15 0.28 1503)
     (b16 0.57 3744)
     (b17 1.12 8391)
     (b18 2.24 14431)
     (b19 4.61 20846)
     (b20 9.64 32411))

   )

  (p
    "All numbers in the above table are the medians of three
consecutive runs after two warm-up runs. The first number is the user
time, in seconds. The number underneath is the number of page
reclaims, as reported by " (code "getrusage()") ". The number of page
reclaims is related (albeit not directly) to the memory
requirements of a process. The user time and the number of page
reclaims reflect the resources used only by the parser. Resources used
during the application start up and loading of the document in memory
are excluded from the reported counts.")

   (p "The timings are precise: they were reported by
 " (code "getrusage()") ", using a microsecond virtual clock. The
system time is insignificant. For example, the system type for the
benchmark string-value-ssax-ss on the file bench1-file20.xml is 0.22
seconds, which is about 2% of the total time. All benchmarks were run
on a FreeBSD 4.6.2-RELEASE system, Pentium IV 2GHz CPU, 1GB RAM. The
numbers above reflect activities that occur entirely in memory. There
is no i/o whatsoever -- neither application i/o nor VM-related
i/o. There were no page faults. The large page-reclaim count for
Scheme programs is most likely due to initial heap allocations. After
a threshold is reached, the heap growth significantly slows down: note a
sublinear increase in page-reclaim counts for Scheme benchmarks when
processing large XML files.")

   (p "We were using Expat library version 1.95.5, installed as a
FreeBSD package. Several other components of FreeBSD depend on that
library. We compiled the C code with gcc 2.95.3, option -O2. We used a
Bigloo 2.4b Scheme system, which was itself compiled with the flags
" (code "-O3 -fomit-frame-pointer -mcpu=i686") ". The complete set of
options and other compilation details is enumerated in Makefile
" (cite "SSAX-benchmarks") ".")

   (p
    "The most striking is the comparison of columns
string-value-by-one and string-value-ssax-ss: A large, practically
relevant " (strong "Scheme") " program, an XML parser, runs measurably
" (strong "faster") " than an equivalent well-written C program!  Some
XML performance benchmarks " (cite "Parser-benchmark") " show that
even the fastest Java XML parser (XP) is slower than Expat by an order
of magnitude; Perl and Python parsers that are based on Expat [sic!]
are slower than Expat by a factor of 20-25 (for large files). Thus the
SSAX parser seems quite competitive in performance. Many thanks to
Manuel Serrano for his excellent optimizing compiler, Bigloo. I must
also stress that SSAX is a pure functional parser.  The whole parser
and all of its handlers are completely referentially transparent.")

   (Section 2 "Why SSAX?")

   (p "Despite some similarities between SSAX and Expat (which came as
a surprise to myself), SSAX is " (em "not") " a Scheme clone of
Expat. SSAX is intended to be a toolkit for various markup-related
tasks. One example -- parsing of (potentially invalid) HTML -- has
been demonstrated recently " (cite "HTML-parsing") ". It was so easy
to write an HTML parser/lexer with SSAX. The major difference between
SSAX and Expat however is that SSAX has a better interface.")

   (Section 3 "SSAX has a better API")

   (p "The application interface of SSAX has several advantages over
Expat API. SSAX minimizes the amount of application-specific state
that has to be shared among user-supplied event handlers. In
particular, SSAX makes the maintenance of an element stack
unnecessary, which eliminates several classes of common bugs. SSAX is
written in a pure-functional subset of Scheme. Therefore, the event
handlers are referentially transparent, which makes them easier for a
programmer to write and to reason about. The more expressive, reliable
and easier to use application interface for the event-driven XML
parsing is the outcome of implementing the parsing engine as an
enhanced tree fold combinator, which fully captures the control
pattern of the depth-first tree traversal.")
   (p
    "A tutorial article about Expat " (cite "Expat-tutorial") "
explains well how Expat is supposed to be used. A user application
most certainly has to have")
   (blockquote
    "a good stack mechanism in order to keep track of current
context... The things you're likely to want to keep on a stack are the
currently opened element and it's attributes. You push this
information onto the stack in the start handler and you pop it off in
the end handler.")
   (p
    "Compare that description with the following SSAX application, which
converts an XML document to a simplified (for the sake of clarity) SXML:")
   (verbatim
    "(define (simple-XML->SXML port)"
    "  (reverse"
    "   ((SSAX:make-parser"
    "     NEW-LEVEL-SEED"
    "     (lambda (elem-gi attributes namespaces expected-content seed)"
    "     '())"
    ""
    "     FINISH-ELEMENT"
    "     (lambda (elem-gi attributes namespaces parent-seed seed)"
    "       (cons (cons elem-gi (reverse seed)) parent-seed))"
    ""
    "     CHAR-DATA-HANDLER"
    "     (lambda (string1 string2 seed)"
    "       (if (string-null? string2) (cons string1 seed)"
    "           (cons* string2 string1 seed)))"
    ""
    "    )"
    "    port '())))"
    )

   "As you see, " (code "simple-XML->SXML") " does not need any stack
to keep track of the current context. There is no need to maintain the
stack across several callbacks and to check for underflows and other
errors. The SSAX callbacks hardly do anything at all.  The simpler the
handlers are, the easier it is to write them and to reason about
them. "




   (Section 2 "Answers to comments")

   (Section 3 "Choice of XML trees as input files for the benchmark")

   (p "XML representation of trees has many deeply-" (em "nested")
" " (em "elements.") "Processing such documents exercises the parser
engine. In contrast, marked-up Shakespeare's plays (another popular
choice for benchmark input files) are comprised mostly of character
data. They do not exert the element parsing engine.")

   (Section 3 "Is the benchmark fair to Expat?")

   (blockquote
    (em "The most striking result is that a Scheme application is only 1.4
times slower than a comparable well-written C application.") (br)
    "Yes, but only if you slow down the C application by a factor of 7 first.")
 
   "Whatever overhead is imposed on " (code "string-value-by-one.c") "
compared to " (code "string-value.c") ", it applies equally to
" (code "string-value-ssax-ss") ". The benchmark does not subject
Expat to more overhead than it is imposed on SSAX. Therefore, the
comparisons of two parsers is fair and meaningful."

   (p "SSAX could have been written like Expat, to accept chunks of
arbitrary size. SSAX could have read the whole document into memory
and parse it from there. As the benchmark shows, parsing from memory
and avoiding any string copies increases the performance by a factor
of 7. Yet I did not feel that solution satisfactory. I do parse XML
documents from pipes; oftentimes the documents are generated on the
fly so I do not know what the final size of the document will be. In
that situation, I can not load the document into memory without
\"pre-parsing\" of the input stream first. Furthermore, SSAX can
handle documents that are simply too big to load into memory.")

  (p "We should remark that if the Expat-like input mode is required
by a particular application, it can be achieved in SSAX. Furthermore,
we can turn SSAX into an XML Pull parser with the help of a general
iteration inversion procedure, as described in " (cite "SSAX-pull")
".")


   (Section 3 "Is SSAX really referentially transparent?")

   (p "It is emphasized in SSAX title comments and elsewhere that the
input port is treated as a linear, read-once parameter. This technique
is similar to that of Clean. I/O in Clean " (em "is") " referentially
transparent.")
   (p "Because the port is treated in SSAX as a read-once parameter,")
   (verbatim
    "(let ((result (foo port))) ...)")
   "is equivalent to"
   (verbatim
    "(let-values (((result new-port) (foo* port))) ... )")
   "and can be " (em "mechanically") " transformed into the latter if
needed. Transformations of that kind cannot generally be performed on
code that uses assignments indiscriminately (which is not
referentially transparent, that is)."
   (p "In short, SSAX is as referentially transparent as Clean is --
which has been proven to be.")


   (Section 3 "What" " about performance of Expat if it were to wrapped
in Scheme?")
   (blockquote
    "Just for the sake of argument, if one were to wrap Expat into
Scheme primitive objects, and invoke Expat from an interpreter, how do
you think it will perform?")
   (p
    "If you invoke Expat from a Scheme interpreter, you
should expect the the performance similar to that of Perl or Python
wrappers around Expat " (cite "Parser-benchmark") ".")
   (p
    "The wrappers start off well for small documents; but as the document
size increases to around 3 MB, the wrapped Expat becomes 20-30 times
slower than the original Expat. The reason is the cost of invoking of
Perl/Python callbacks from the Expat code and the related cost of
translating strings and other data across the language barrier.")


   (Section 2 "References")

   (bibitem "Expat-tutorial" "Expat-tutorial"
	    "Clark Cooper. Using Expat. September 01, 1999."
	    (URL "http://www.xml.com/pub/a/1999/09/expat/index.html"))

   (bibitem "SSAX" "SSAX"
	    "SSAX and SXML at SourceForge."
	    (URL "http://ssax.sourceforge.net/"))

   (bibitem "SSAX-benchmarks" "SSAX-benchmarks"
	    "SSAX and Expat benchmarks."
	    (URL "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ssax/SSAX/benchmarks/"))


   (bibitem "Parser-benchmark" "Parser-benchmark"
	    "Clark Cooper. Summary of XML Parser Performance
Testing. May 05, 1999."
	    (URL "http://www.xml.com/pub/a/Benchmark/exec.html"))

   (bibitem "HTML-parsing" "HTML-parsing"
     (a (@ (href "xml.html#HTML-parser"))
       "Permissive parsing of perhaps invalid HTML"))

   (bibitem "SSAX-pull" "SSAX-pull" 
     (a (@ (href "xml.html#SSAX-pull"))
       "XML pull parsing and SSAX"))

   (bibitem "Expat-vs-SSAX thread" "Expat-vs-SSAX thread"
      (Cite "Expat vs. SSAX, C vs. Scheme (Bigloo)")
      "Two articles posted on a newsgroup comp.lang.scheme on Thu, 1 Nov 2001 13:41:22 -0800 and Mon, 5 Nov 2001 13:40:16 -0800.")

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
     ; lay out the table, horizontally
     `((benchmark-table
	*macro*
	. ,(lambda (tag files legend . results)
	     (define (print-num n) ; int -> list of chars, in groups of three
	       (let loop ((digits-rev 	; interspersed with commas
			    (reverse (string->list (number->string n))))
			  (grouped-by-three '()))
		 (if (null? digits-rev)
		   (list-intersperse grouped-by-three #\,)
		   (let* ((group (list (car digits-rev)))
			  (lst (cdr digits-rev))
			  (group (if (null? lst) group (cons (car lst) group)))
			  (lst (if (null? lst) lst (cdr lst)))
			  (group (if (null? lst) group (cons (car lst) group)))
			  (lst (if (null? lst) lst (cdr lst))))
		     (loop lst (cons group grouped-by-three))))))
	     ; file-desc is (id filename filesize)
	     (define (make-row file-desc)
	       (let*-values
		 (((id filename filesize) (apply values file-desc)))
		 `(tr (td (@ (align right)) ,filename (br) 
			(code ,(print-num filesize)) " bytes")
		    ,@(map 
			(lambda (result-record)
			  (let*-values
			    (((id time space)
			       (apply values (assq id (cddr result-record)))))
			    `(td (@ (align right)) 
			       (code ,time "s") (br) (n_) (code ,space (n_)))))
			results))))
	     (assert (eq? 'files (car files)) (eq? 'legend (car legend))
	       (eq? 'result (caar results)))
	     `(table (@ (align center) (cellpadding 3) (cellspacing 9))
		(tr (th (n_)) 
		  ,@(map (lambda (res) `(th ,(cadr res))) results))
		,@(map make-row (cdr files)))
	     )))
     ))))

(generate-HTML Content)
