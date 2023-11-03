  /*
    *  The scanner definition for COOL.
  */

  /*
    *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
    *  output, so headers and global definitions are placed here to be visible
    * to the code in the file.  Don`t remove anything that was here initially
  */

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylex  cool_yylex
#define yylval cool_yylval
/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int nr_whitespaces = 0, nr_letter = 0;

%}

  /*
    * Define names for regular expressions here.
  */

NEW_LINE \n
WHITESPACE  [\t\n\v\f\r ]

CLASS ?i:class 

ELSE ?i:else 
FI ?i:fi 
IF ?i:if 

IN ?i:in 

INHERITS ?i:inherits
LET ?i:let

LOOP ?i:loop
POOL ?i:pool
THEN ?i:then
WHILE ?i:while

CASE ?i:case 
ESAC ?i:esac 

OF ?i:of 

DARROW => 

NEW ?i:new 
ISVOID ?i:isvoid 

STR_CONST \"(\\[^btnf\"\\]|[^\n\"\\])*\" 
INT_CONST -?[0-9]+ 

TRUE_CONST  t[Rr][Uu][Ee] 
FALSE_CONST f[Aa][Ll][Ss][Ee] 

TYPEID [A-Z][A-Za-z0-9_]* 
OBJECTID [a-zA-Z][a-zA-Z0-9_]* 

LE le 

ASSIGN <- 
NOT ?i:not 

BRACKETS [{}()]

SEMICOLON ;
COMMA ,

DOT "."
TWO_DOTS ":"
AT "@"
TILDE "~"

PLUS "+"
MINUS "-"

DEVISION "/"
PRODUCT "*"

COMPARISON_LARGER "<"
COMPARISON_EQUAL "="
COMPARISON_LARGER_OR_EQUAL "<="

ERROR_IGNORE error 
ERROR . 

%x IN_COMMENT IN_COMMENT_2

%%

{WHITESPACE} { 
  if (*yytext == '\n') {
    curr_lineno++;
  }
}

{ERROR_IGNORE} { /* Do nothing */ }

 /* Keywords */
{CLASS}      { return (CLASS); }
{ELSE}       { return (ELSE); } 
{FI}         { return (FI); } 
{IF}         { return (IF); } 
{IN}         { return (IN); } 
{INHERITS}   { return (INHERITS); }
{LET}        { return (LET); }
{LOOP}       { return (LOOP); } 
{POOL}       { return (POOL); }
{THEN}       { return (THEN); }
{WHILE}      { return (WHILE); } 
{CASE}       { return (CASE); }
{ESAC}       { return (ESAC); } 
{OF}         { return (OF); }
{NEW}        { return (NEW); } 
{ISVOID}     { return (ISVOID); }
{ASSIGN}     { return (ASSIGN); }
{NOT}        { return (NOT); }
{DARROW}     { return (DARROW); }

{TRUE_CONST}  { 
  cool_yylval.boolean = true;
  return (BOOL_CONST); 
}
{FALSE_CONST}  { 
  cool_yylval.boolean = false;
  return (BOOL_CONST); 
}

 /* 
  * Symbols - brackets, semicolon, operands
  */
{BRACKETS} { return *yytext; }

{SEMICOLON} { return *yytext; }
{COMMA} { return *yytext; }

{DOT} { return *yytext; }
{TWO_DOTS} { return *yytext; }

{AT} { return *yytext; }
{TILDE} { return *yytext; }

{PLUS} { return *yytext; }
{MINUS} { return *yytext; }

{DEVISION} { return *yytext; }
{PRODUCT} { return *yytext; }

{COMPARISON_LARGER} { return *yytext; }
{COMPARISON_EQUAL} { return *yytext; }
{COMPARISON_LARGER_OR_EQUAL} { return *yytext; }

 /* CONSTANTS */

{STR_CONST} { 
  // TODO
}

{INT_CONST} { 
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST);
}

 /* IDENTIFIERS */
{TYPEID}  {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return (TYPEID);
}

{OBJECTID} {
  cool_yylval.symbol = stringtable.add_string(yytext);
  return (OBJECTID);
}

 /* ERROR */
{ERROR} { 
  char* error_msg = "Cannot match the symbol passed!";
  cool_yylval.error_msg = yytext;
  return (ERROR); 
}

"(*" BEGIN(IN_COMMENT);
"--" BEGIN(IN_COMMENT_2);

<IN_COMMENT>[^*\n]*        /* eat anything that's not a '*' */
<IN_COMMENT>"*"+[^*/\n]*   /* eat up '*'s not followed by '/'s */
<IN_COMMENT>\n             ++yylineno;
<IN_COMMENT>"*"+")"        BEGIN(INITIAL);

<IN_COMMENT_2>[^*\n]*       /* eat anything that's not a '/n' */
<IN_COMMENT_2>\n             BEGIN(INITIAL);

%%
