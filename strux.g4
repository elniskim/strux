grammar strux;

program
 : declList EOF
 ;

declList
 : decl*
 ;

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
 : Identifier '[' Integer ']' ':' type ';'
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

stmtBlock
 : '{' stmtList '}'
 ;

stmtList
 : stmt*
 ;

stmt
 : varDecl
 | designatorStmt
 | assignStmt
 | ifStmt
 | forStmt
 | whileStmt
 | returnStmt
 | breakStmt
 | continueStmt
 ;

assignStmt
 : assignStmtNoSemi ';'
 ;

assignStmtNoSemi
 : designator '=' expr0
 ;
// We have to check for L-Value-ness during type checking now

designatorStmt
 : designator ';'
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
 : 'for' '(' (assignStmt | ';') (expr0)? ';' (assignStmtNoSemi)? ')' stmtBlock
 ;

whileStmt
  : 'while' '(' expr0 ')' stmtBlock
  ;

exprList
 : ( expr0 ( ',' expr0 )* )?
 ;

expr0
 : expr1
 | expr0 op0 expr1
 ;

expr1
 : expr2
 | expr1 op1 expr2
 ;

expr2
 : expr3
 | expr2 op2 expr3
 ;

expr3
 : '!' expr3
 | '(' expr0 ')'
 | designator
 | literal
 ;

op0
 : '>='
 | '<='
 | '=='
 | '!='
 | '>'
 | '<'
 ;

op1
 : '+'
 | '-'
 | 'or'
 ;

op2
 : '*'
 | '/'
 | '%'
 | 'and'
 ;

designator
 : Identifier postfixOp*
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
