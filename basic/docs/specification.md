## Supported features

  * PRINT expression
  * INPUT variable
  * REM comment
  * LET variable = expression
  * GOTO line number
  * IF expression THEN line number

## EBNF grammar

```
  UNARY_OP ::= - | !

  BINARY_OP ::= + | - | * | / | % | = | <> | < | <= | > | >= | & | '|' 

  variable ::= string

  EXPRESSION ::= integer
              | variable
              | "string"
              | UNARY_OP EXPRESSION
              | EXPRESSION BINARY_OP EXPRESSION
              | ( EXPRESSION )

  COMMAND ::= REM string
            | INPUT variable
            | PRINT EXPRESSION
            | LET variable = EXPRESSION
            | GOTO integer
            | IF EXPRESSION THEN integer

  LINE ::= integer COMMAND

  PROGRAM ::= LINE | LINE PROGRAM

  PHRASE ::= LINE | RUN | LIST | END

```