/*****************************************************************
 * string-value-by-one.c
 *
 * An Expat benchmark test
 * Computing the string value of the root node of the DOM tree
 * of the XML document.
 *
 * An XML document is loaded into memory first (from a file whose
 * name is the single argument to this program). We then pass the
 * content of that buffer one character at a time to Expat.
 * This simulates the work of the SSAX parser.
 *
 * The computed string value is disregarded.
 *
 * $Id: string-value-by-one.c,v 1.1.1.1 2001/11/02 22:57:08 oleg Exp $
 */


#include <stdio.h>
#include <expat.h>
#include <assert.h>

extern void rusage_start(void);
extern void rusage_report(char * title);

/* Buffer to accumulate the string value in */
struct char_buf {
  int cur_len;
  int alloc_len;
  char * data;		/* Not null-terminated! */
};

static void char_data(void *userData,
		      const XML_Char *s,
		      int len)
{
  struct char_buf * outs = (struct char_buf *)userData;
  if( outs->cur_len + len >= outs->alloc_len )
  {				/* Reallocate the data buffer */
    outs->alloc_len *= 2;		/* double the size */
    assert( outs->data = (char *)realloc(outs->data,outs->alloc_len) );
  }
  memcpy(outs->data + outs->cur_len,s,len);
  outs->cur_len += len;
}


main(int argc, char **argv) {
  XML_Parser p = XML_ParserCreate(NULL);
  FILE * fp = (FILE*)0;
  char * buffer = 0, *bp = 0;
  struct char_buf outs;
  int len;

  if (! p) {
    fprintf(stderr, "Couldn't allocate memory for parser\n");
    exit(-1);
  }

  if( argc != 1 + 1 )
    fprintf(stderr, "One argument required: the name of the file to parse\n"),
      exit(4);

  if( (fp = fopen(argv[1],"r")) == (FILE*)0 )
    perror("File open error"),
      exit(4);

  
  fseek(fp,0,SEEK_END);
  len = ftell(fp);
  rewind(fp);

  fprintf(stderr,"\nLoading the whole file of %d bytes into the buffer\n",
	  len);
  assert( buffer = (char*)malloc(len) );
  assert( fread(buffer,1,len,fp) == len );
  fprintf(stderr,"\nLoaded the whole file\n");

  outs.cur_len = 0;
  outs.alloc_len = 16;
  assert( outs.data = (char *)malloc(outs.alloc_len) );


  XML_SetCharacterDataHandler(p,char_data);
  XML_SetUserData(p,&outs);

  rusage_start();
  
  for(bp=buffer; bp<buffer+len; bp++)
    if (! XML_Parse(p, bp, 1, bp == (buffer+len-1))) {
      fprintf(stderr, "Parse error at line %d:\n%s\n",
	      XML_GetCurrentLineNumber(p),
	      XML_ErrorString(XML_GetErrorCode(p)));
      exit(-1);
    }
  rusage_report("Resource usage");

  if( outs.cur_len >= 15 )
    fprintf(stderr,"The first 15 (out of %d) bytes of outs.data: '%.15s'\n",
	    outs.cur_len,outs.data);
  fprintf(stderr,"\nDone\n");
  return 0;
}

