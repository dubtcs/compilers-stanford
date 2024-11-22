/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}

    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/

    /* Bullshit I don't feel like writing every time. ASCII codes. */
    %token LPAREN 40
    %token RPAREN 41
    %token LBRACE 123
    %token RBRACE 125
    %token LBRACKET 133
    %token RBRACKET 135
    %token COL 58
    %token SEM 59
    %token COMMA 44

    %token PLUS 43
    %token MINUS 45
    %token MULT 42
    %token DIVIDE 47

    %token TILDE 126
    %token AT 64
    %token DOT 46

    %token LESS 60
    %token EQUAL 61

    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type<program> program
    %type<classes> class_list
    %type<class_> class
    
    %type<features> feature_list
    %type<feature> feature

    %type<expressions> expression_list
    %type<expressions> expression_set
    %type<expression> expression
    %type<expression> let_exp
    %type<cases> case_list
    %type<case_> case_branch

    %type<formals> formal_list
    %type<formal> formal

    /* Precedence declarations go here. */
    %left PLUS
    %left MINUS
    %left MULT
    %left DIVIDE

    %left TILDE
    %left AT
    %left DOT
    
    %left ASSIGN
    %left NOT
    %left ISVOID

    %nonassoc LE
    %nonassoc LESS
    %nonassoc EQUAL

    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    // program ::= [class;]+
    program	: 
    class_list { 
      @$ = @1;
      ast_root = program($1); 
    }
    ;
    
    // class_list ::= class+
    class_list : 
    class SEM {			/* single class */ 
      $$ = single_Classes($1);  // single_[phylums] creates a list of length 1 of type [phylums]
      parse_results = $$;       // returns said list
    }
    | class_list class {	/* several classes */
      $$ = append_Classes($1,single_Classes($2)); // see above, this time appending the current instance to the recursive result
      parse_results = $$;
    }
    | error class {
      $$ = single_Classes($2);
      parse_results = $$;
    }
    ;

    // class ::= class TYPE [inherits TYPE] { [feature;]* } ; <- this semicolon is handled by class_list
    class	:
    CLASS TYPEID LBRACE feature_list RBRACE { 
      $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename));
    }
    | CLASS TYPEID INHERITS TYPEID LBRACE feature_list RBRACE { 
      $$ = class_($2, $4, $6, stringtable.add_string(curr_filename)); 
    }
    ;

    // feature_list ::= feature+
    feature_list :
    {
      // no recognized pattern, empty set
      $$ = nil_Features();
    }
    | feature_list feature {
      // jesus fucking christ, flagging an error here saying "feature_list" has no declared type
      // error was fucking useless. Real problem was the line below
      // where single_Features($2) was pointing to object 2 which is a semicolon
      $$ = append_Features($1, single_Features($2));
    }
    | feature SEM { // semicolon rule here bc all features in a feature_list require a semicolon after
      $$ = single_Features($1);
    }
    ;

    // feature can be either attribute or method
    // feature ::= ID( formal+ ) : TYPE { expr }
    // named references aren't working FUCKING SHIT
    feature :
    OBJECTID LPAREN formal_list RPAREN COL TYPEID LBRACE expression RBRACE {
      // method
      $$ = method($1, $3, $6, $8);
    }
    | OBJECTID COL TYPEID ASSIGN expression { // TYPE : ID <- expr+
      // constructor/assignment
      $$ = attr($1, $3, $5);
    }
    | OBJECTID COL TYPEID {
      $$ = attr($1, $3, no_expr());
    }
    ;

    formal_list :
    {
      $$ = nil_Formals();
    }
    | formal {
      $$ = single_Formals($1);
    }
    | formal_list COMMA formal { // comma rule here bc the comma is required for a single formal
      $$ = append_Formals($1, single_Formals($3));
    }

    formal :
    OBJECTID COL TYPEID {
      $$ = formal($1, $3);
    }

    // Cool manual page 7
    // Especially helpful for static vs normal dispatch (sec 7.4, page 10)
    // Dispatches:
    /* <expr>.<id>(e0...en-1) OOP style call
     *    It takes the final class type from <expr>, binds <id> to the "self" parameter, and calls it with arguments.
     *    Like I said, standard OOP type shi
     * 
     * <id>(e0...en-1) OOP style self referencial call
     *    Just like previous, but is just shorthand for self.<id>(...)
     * 
     * <expr>@<type>.id(...)
     *    Way to call methods from other definitions with the current id as a paramemter
     *    eg, e@b.f() uses the b class definition of the method f on e
    */
    expression :
    OBJECTID ASSIGN expression {
      // ID <- expr
      $$ = assign($1, $3);
    }
    | expression DOT OBJECTID LPAREN expression_list RPAREN {
      // expr.ID(expr*)
      $$ = dispatch($1, $3, $5);
    }
    | expression AT TYPEID DOT OBJECTID LPAREN expression_list RPAREN {
      // expr@TYPE.ID(expr*)
      $$ = static_dispatch($1, $3, $5, $7);
    }
    | OBJECTID LPAREN expression_list RPAREN {
      // ID(expr*)
      $$ = dispatch(object(idtable.add_string("self")), $1, $3);
    }
    | IF expression THEN expression ELSE expression FI {
      $$ = cond($2, $4, $6);
    }
    | WHILE expression LOOP expression POOL {
      $$ = loop($2, $4);
    }
    | LBRACKET expression_set RBRACKET {
      $$ = block($2);
    }
    | LET let_exp {
      // let statement in 7.8 page 11
      $$ = $2;
    }
    | CASE expression OF case_list ESAC {
      $$ = typecase($2, $4);
    }
    | NEW TYPEID {
      $$ = new_($2);
    }
    | ISVOID expression {
      $$ = isvoid($2);
    }
    | expression PLUS expression {
      $$ = plus($1, $3);
    }
    | expression MINUS expression {
      $$ = sub($1, $3);
    }
    | expression MULT expression {
      $$ = mul($1, $3);
    }
    | expression DIVIDE expression {
      $$ = divide($1, $3);
    }
    | TILDE expression {
      $$ = neg($2);
    }
    | expression LESS expression {
      $$ = lt($1, $3);
    }
    | expression LE expression {
      $$ = leq($1, $3);
    }
    | expression EQUAL expression {
      $$ = eq($1, $3);
    }
    | NOT expression {
      $$ = comp($2);
    }
    | LPAREN expression RPAREN {
      $$ = $2;
    }
    | OBJECTID {
      $$ = object($1);
    }
    | INT_CONST {
      $$ = int_const($1);
    }
    | STR_CONST {
      $$ = string(const_$1);
    }
    | BOOL_CONST {
      $$ = bool_const($1);
    }

    case_list:
    case_branch {
      $$ = single_Cases($1);
    }
    | case_list case_branch {
      $$ = append_Cases($1, $2);
    }

    case_branch:
    OBJECTID COL TYPEID DARROW expression SEM {
      $$ = branch($1, $3, $5);
    }

    let_exp:
    OBJECTID COL TYPEID IN expression{
      $$ = let($1, $3, no_expr(), $5);
    }
    | OBJECTID COL TYPEID COMMA let_exp {
      $$ = let($1, $3, no_expr(), $5);
    }
    | OBJECTID COL TYPEID ASSIGN expression IN expression {
      $$ = let($1, $3, $5, $7);
    }
    | OBJECTID COL TYPEID ASSIGN expression COMMA let_exp {
      $$ = let($1, $3, $5, $7);
    }

    // comma sep
    expression_list:
    expression {
      $$ = single_Expressions($1);
    }
    | expression_list COMMA expression {
      $$ = append_Expressions($1, single_Expressions($3));
    }

    // semicolon sep
    expression_set:
    expression SEM {
      $$ = single_Expressions($1);
    }
    | expression_set expression {
      $$ = append_Expressions($1, single_Expressions($2));
    }

    // See section 6 of the cool tour handout for info about the absract syntax trees

    // https://www.gnu.org/software/bison/manual/bison.html#Semantic-Values
    // $x -- the semantic info, as in the specific data of THAT token. eg the value of an integer or name of an identifier
    // So:      5 + 13
    //          exp + exp {
    //            $$ = $1 + $3
    //          }
    // translates to (result = 5 + 13) where result is $$, 5 is $1, and $3 is 13
    
    // https://www.gnu.org/software/bison/manual/bison.html#Actions
    // $$ refers to the semantic value of the group currently being constructed, see above

    // https://www.gnu.org/software/bison/manual/bison.html#Actions-and-Locations
    // Use @x to access the location of an element
    // this is quite literally the location of the element
    // like, literally the col:line number in the source file

    /* If no parent is specified, the class inherits from the Object class. */
    
    /* Feature list may be empty, but no empty features in list. */
    
    
    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    