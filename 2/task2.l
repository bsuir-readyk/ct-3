%option noyywrap

%{
#include <stdio.h>
%}

%%

[A-Z] {
    char c = yytext[0];
    c = 'A' + ((c - 'A' + 3) % 26);
    putchar(c);
}

[a-z] {
    char c = yytext[0];
    c = 'a' + ((c - 'a' + 3) % 26);
    putchar(c);
}

[^A-Za-z] {
    ECHO;
}

%%
