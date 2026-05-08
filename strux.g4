grammar strux;

program
 : declList EOF
 ;

declList
 : decl*
 ;

// --- Declarations ---
decl
 : varDecl
 | arrayDecl
 | funcDef
 | structDef
 ;

varDecl
 : Identifier ':' type ';'
 ;

arrayDecl
 : Identifier ':' type '[' Integer ']' ';'
 ;

funcDef
 : 'def' Identifier '(' paramList ')' '->' type stmtBlock
 ;

structDef
  : 'struct' Identifier '{' declList '}'
  ;

paramList
 : (param (',' param)* )?
 ;

param
 : Identifier ':' type
 ;

// --- Statements ---
stmtBlock
 : '{' stmtList '}'
 ;

stmtList
 : stmt*
 ;

stmt
 : varDecl
 | arrayDecl
 | exprStmt
 | ifStmt
 | forStmt
 | whileStmt
 | returnStmt
 | breakStmt
 | continueStmt
 ;

exprStmt
 : expr0 ';'
 ;

returnStmt
 : 'return' expr0 ';'
 ;

breakStmt
 : 'break' ';'
 ;

continueStmt
 : 'continue' ';'
 ;

ifStmt
 : 'if' expr0 stmtBlock ('else' stmtBlock)?
 ;

forStmt
 : 'for' '(' (expr0)? ';' (expr0)? ';' (expr0)? ')' stmtBlock
 ;

whileStmt
  : 'while' '(' expr0 ')' stmtBlock
  ;

exprList
 : ( expr0 ( ',' expr0 )* )?
 ;

// --- Expressions ---

// Assignment
expr0
 : expr1 '=' expr0
 | expr1 
 ;

// Comparisons
expr1
 : expr2 (op1 expr2)*
 ;

// Additive operators
expr2
 : expr3 (op2 expr3)*
 ;

// Multiplicative operators
expr3
 : expr4 (op3 expr4)*
 ;

// Unary operators
expr4
 : '-' expr4
 | '!' expr4
 | expr5
 ;

expr5
 : primary (postfixOp)*
 ;

primary
 : '(' expr0 ')' 
 | Identifier 
 | literal 
 ;

op1
 : '>='
 | '<='
 | '=='
 | '!='
 | '>'
 | '<'
 ;

op2
 : '+'
 | '-'
 | '|'
 | '||'
 ;

op3
 : '*'
 | '/'
 | '%'
 | '&'
 | '&&'
 ;

postfixOp
  : '[' expr0 ']'
  | '->' Identifier
  | '(' exprList ')'
  ;

type
 : Identifier
 ;

literal
 : Integer
 | True
 | False
 ;

Integer
 : '0'
 | [1-9] [0-9]*
 ;

True: 'True';
False: 'False';

Identifier
 : [a-zA-Z] [a-zA-Z0-9_]*
 ;

WhiteSpaces
 : [ \t\r\n]+ -> skip
 ;

Comment
 : '//' ~[\r\n]* -> skip
 ;
