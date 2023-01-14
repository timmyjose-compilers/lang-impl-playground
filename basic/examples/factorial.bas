5 REM A simple factorial program
10 PRINT " factorial of?"
20 INPUT A
30 LET B = 1
35 REM beginning of loop
40 IF A <= 1 THEN 80
50 LET B = B * A
60 LET A = A - 1
70 GOTO 40
75 REM print the result
80 PRINT B