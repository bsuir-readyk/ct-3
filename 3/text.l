%option main
%option noyywrap
%option noinput
%option nounput

NEXT    \n|\r\n|\t
STRING  \"[^\"]*\"
DIGIT   [1-9]

%%

{NEXT}      { printf("0\n"); }

[ ]         { printf("1\n"); }

"-module"   { printf("2\n"); }

"-export"   { printf("3\n"); }

[(]         { printf("4\n"); }

[)]         { printf("5\n"); }

"["         { printf("6\n"); }

"]"         { printf("7\n"); }

"/"         { printf("8\n"); }

"0"         { printf("9\n"); }

main        { printf("10\n"); }

"->"        { printf("11\n"); }

"."         { printf("12\n"); }

"io:format" { printf("13\n"); }

{STRING}    { printf("14\n"); }

","         { printf("15\n"); }

if          { printf("16\n"); }

{DIGIT}     { printf("17\n"); }

end         { printf("18\n"); }

";"         { printf("19\n"); }

true        { printf("20\n"); }

ok          { printf("21\n"); }

%%

