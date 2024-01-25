grammar Grammar;

start : name define term+ rule+ EOF;

// Set rule as start.
define : 'start' 'is' RULE SEMICOLON;

// Name for grammatics and generated Lexer, Parser, Contexts.
name : GRAMMAR TERMINAL SEMICOLON;

// Terminals.
// May be as real term (string) or as Java's regex.
term : TERMINAL ASSIGN (STRING | REGEX) SEMICOLON;

// Rules.
// May be with attributes (arguments and returns).
rule : RULE ruleL ASSIGN ruleR SEMICOLON;
ruleL : args? ret?;
ruleR : stmts (VERTSLASH stmts)*;
args : LBRT arg (COMMA arg)* RBRT;
arg : argType argName;
argType : RULE | TERMINAL;
argName : RULE;
ret : ARROW arg;
stmts : stmt+;
stmt : TERMINAL CODE? | RULE ARGS? CODE?;

// Constants.
ASSIGN : ':';
SEMICOLON : ';';
GRAMMAR : 'grammar';
LBRT : '<';
RBRT : '>';
COMMA : ',';
ARROW : '->';
VERTSLASH : '|';
QUOTE : '\'';

// Constructions.
CODE : '~' .+? '~';
ARGS : '@' .+? '@';
REGEX : '/' .+? '/';
STRING : QUOTE (~'\'')+ QUOTE;
RULE : LOWERCASE IDENTIFIER;
TERMINAL : UPPERCASE IDENTIFIER;
DIGIT : [0-9];
LOWERCASE : [a-z];
UPPERCASE : [A-Z];

// Technical.
fragment IDENTIFIER : (UPPERCASE | LOWERCASE | DIGIT)*;
WS : [ \t\n\r] -> skip;
