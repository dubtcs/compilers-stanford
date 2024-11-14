/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
/* FUCK THIS. I am NOT using macros for variable access. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

// ::Error Macros::
// Use the ERR_ macros as arguments to the PXXX(s) macros.

// Use this to dump an error. Be sure to semicolon after
#define PERROR(s) BEGIN(INITIAL); cool_yylval.error_msg = s; return ERROR

// Push an error without reverting state. Used in instances where parsing must continue
#define PWARN(s) cool_yylval.error_msg = s; return ERROR

// I got tired of writing this.
// Use these error messages for logging to get grading points

#define ERR_STR_SIZE "String constant too long"
#define ERR_STR_NEWLINE "Unterminated string constant"
#define ERR_STR_NULL "String contains null character"
#define ERR_STR_EOF "EOF in string constant"

#define ERR_COMMENT_EOF "EOF in comment"
#define ERR_COMMENT_CLOSER "Unmatched *)"

#define ERR_CHAR_INVALID yytext



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

/* DECLARATIONS */

/*  stringtable is for string constants   List<StringEntry>
 *  inttable is for numeric constants     List<IntEntry>
 *  idtable is for identifiers            List<IdEntry>
 */

bool PushChar(char c);
bool StrCheck();

int commentDepth = 0;

%}

%x MULTI_COMMENT
%x STRING_CONST
%x STRING_CONST_INVALIDATED

DIGIT         [0-9]+
NULLC         \0
WHITE         [ \f\r\t\v]*

ID_TYPE       [A-Z][_0-9a-zA-Z]*
ID_OBJ        [a-z][_0-9a-zA-Z]*

DQUOTE        \"
MULTI_START   \(\*
MULTI_END     \*\)
COMMENT       --.*\n
COMMENTEOF    --.*

KW_TRUE       t(?i:rue)
KW_FALSE      f(?i:alse)

KW_CLASS      (?i:class)
KW_IF         (?i:if)
KW_ELSE       (?i:else)
KW_FI         (?i:fi)
KW_INHERITS   (?i:inherits)
KW_ISVOID     (?i:isvoid)
KW_LET        (?i:let)
KW_LOOP       (?i:loop)
KW_POOL       (?i:pool)
KW_THEN       (?i:then)
KW_WHILE      (?i:while)
KW_CASE       (?i:case)
KW_ESAC       (?i:esac)
KW_NEW        (?i:new)
KW_OF         (?i:of)
KW_NOT        (?i:not)
KW_IN         (?i:in)

LPAREN        (
RPAREN        )
DOT           \.
AT            @
TILDE         ~
MULT          \*
DIV           \/
PLUS          \+
MINUS         \-
ASSIGN        <-
LESSEQ        <=
DARROW        =>
SENDAS        [<@~=.,\-\+\*\/{}():;]

%%

<INITIAL>{
  {WHITE} {}
  {COMMENT} { curr_lineno++; }
  {COMMENTEOF} { // end of file comment, don't increase line number }
  \n { curr_lineno++; }
  {MULTI_START} {
    commentDepth++;
    BEGIN(MULTI_COMMENT);
  }
  {DQUOTE} {
    string_buf_ptr = string_buf;
    BEGIN(STRING_CONST);
  }
  {MULTI_END} {
    PERROR(ERR_COMMENT_CLOSER);
  }
  {DIGIT} {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
  }

  {KW_TRUE} {
    cool_yylval.boolean = true;
    return BOOL_CONST;
  }
  {KW_FALSE} {
    cool_yylval.boolean = false;
    return BOOL_CONST;
  }

  {ASSIGN}      { return ASSIGN; }
  {DARROW}      { return DARROW; }
  {LESSEQ}      { return LE; }
  
  {KW_CLASS}    { return CLASS; }
  {KW_IF}       { return IF; }
  {KW_ELSE}     { return ELSE; }
  {KW_FI}       { return FI; }
  {KW_INHERITS} { return INHERITS; }
  {KW_ISVOID}   { return ISVOID; }
  {KW_LET}      { return LET; }
  {KW_LOOP}     { return LOOP; }
  {KW_POOL}     { return POOL; }
  {KW_THEN}     { return THEN; }
  {KW_WHILE}    { return WHILE; }
  {KW_CASE}     { return CASE; }
  {KW_ESAC}     { return ESAC; }
  {KW_NEW}      { return NEW; }
  {KW_OF}       { return OF; }
  {KW_NOT}      { return NOT; }
  {KW_IN}       { return IN; }

  {ID_TYPE} {
    cool_yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
  }
  {ID_OBJ} {
    cool_yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
  }

  {SENDAS} {
    return *yytext;
  }
  
  . {
    PERROR(ERR_CHAR_INVALID);
  }
}

<STRING_CONST_INVALIDATED>{
  \n|{DQUOTE} {
    BEGIN(INITIAL);
  }
  . {}
}

<STRING_CONST>{
  <<EOF>> {
    PERROR(ERR_STR_EOF);
  }
  {DQUOTE} {
    BEGIN(INITIAL);
    cool_yylval.symbol = stringtable.add_string(string_buf);
    string_buf_ptr = 0;
    return STR_CONST;
  }
  \n {
    curr_lineno++;
    PERROR(ERR_STR_NEWLINE);
  }
  \0 {
    //BEGIN(STRING_CONST_INVALIDATED);
    PWARN(ERR_STR_NULL);
  }
  \\0 {
    BEGIN(STRING_CONST_INVALIDATED);
    PWARN(ERR_STR_NULL);
  }
  \\[btnf] {
    char c = '\0';
    switch(yytext[1])
    {
      case 'b': c = '\b'; break;
      case 'f': c = '\f'; break;
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
    }
    if(!PushChar(c))
    {
      BEGIN(STRING_CONST_INVALIDATED);
      PWARN(ERR_STR_SIZE);
    }
  }
  \\[^0] {
    // just push the character after the escaper
    if(!PushChar(yytext[1]))
    {
      BEGIN(STRING_CONST_INVALIDATED);
      PWARN(ERR_STR_SIZE);
    }
  }
  . {
    if(!PushChar(*yytext))
    {
      BEGIN(STRING_CONST_INVALIDATED);
      PWARN(ERR_STR_SIZE);
    }
  }
}

<MULTI_COMMENT>{
  <<EOF>> {
    PERROR(ERR_COMMENT_EOF);
  }
  \n {
    curr_lineno++;
  }
  {MULTI_START} {
    commentDepth++;
  }
  {MULTI_END} {
    commentDepth--;
    if(commentDepth == 0)
    {
      BEGIN(INITIAL);
    }
  }
  . {}  
}

%%

bool StrCheck()
{
  return (string_buf_ptr - string_buf) < MAX_STR_CONST;
}

bool PushChar(char c)
{
  if(!StrCheck())
  {
    return false;
  }
  *string_buf_ptr = c;
  string_buf_ptr++;
  if(StrCheck())
  {
    *string_buf_ptr = '\0';
  }
  return true;
}
