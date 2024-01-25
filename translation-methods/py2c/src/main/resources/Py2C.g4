grammar Py2C;

python: statement* EOF;

statement: assignmentStmt | callStmt | compoundStmt;

compoundStmt: ifStmt | whileStmt | defStmt | forStmt;

defStmt: 'def' target '(' defArgs ')' ':' block;
defArgs: (IDENTIFIER (',' IDENTIFIER)*)?;

block: (TAB statement)+;

ifStmt: 'if' expression ':' block elifStmt* elseStmt?;
elifStmt: TAB? 'elif' expression ':' block;
elseStmt: TAB? 'else' ':' block;
whileStmt: 'while' expression ':' block;

forStmt: 'for' target 'in' (range1Stmt | range2Stmt) ':' block;
range1Stmt: 'range' '(' expression ')';
range2Stmt: 'range' '(' expression ',' expression ')';

callStmt: IDENTIFIER '(' callArgs ')';
callArgs: (expression (',' expression)*)?;

assignmentStmt: target '=' expression;
target: IDENTIFIER;

expression: expressionComp;
expressionComp: expressionBitOr (S_COMP expressionBitOr)?;
expressionBitOr: expressionBitXor (S_BIT_OR expressionBitXor)*;
expressionBitXor: expressionBitAnd (S_BIT_XOR expressionBitAnd)*;
expressionBitAnd: expressionAddSub (S_BIT_AND expressionAddSub)*;
expressionAddSub: expressionMulDiv (sAddSub expressionMulDiv)*;
expressionMulDiv: expressionPow (sMulDiv expressionPow)*;
expressionPow: expressionAtom (S_POW expressionPow)?;
expressionAtom: atom | '(' expression ')';
atom: NUMBER | STRING | TRUE | FALSE | IDENTIFIER;

S_POW: '**';
S_MUL: '*';
S_DIV: '/';
sMulDiv: (S_MUL | S_DIV);
S_ADD: '+';
S_SUB: '-';
sAddSub: (S_ADD | S_SUB);
S_COMP: ('!=' | '==' | '<=' | '>=' | '>' | '<');
S_BIT_AND: '&';
S_BIT_XOR: '^';
S_BIT_OR: '|';

TRUE: 'True';
FALSE: 'False';

STRING: ('"' [a-zA-Z0-9 ]* '"') | ('\'' [a-zA-Z0-9 ]* '\'');
NUMBER: '-'? [0-9]+ ('.' [0-9]+)?;
IDENTIFIER: [A-Za-z0-9_]+;
WS: [ \n\r]+ -> skip;
TAB: '\t' | '\t\t';