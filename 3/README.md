# Task 3 - вариант 16

Для заданного вариантом языка разработать:
•	лексический анализатор, распознающий необходимые лексемы;
•	грамматику языка, включающую в себя базовые конструкции языка;
•	программное средство, проверяющее исходный код программы на соответствие грамматике.

Лексический анализатор можно разработать вручную или использовать утилиты-генераторы. Поддерживаемое подмножество языка согласовать с преподавателем.

**Erlang** (упрощённое подмножество)

## Поддерживаемая грамматика

```
program   →  -module(main). -export([main/0]). main() -> body.
body      →  operator (, operator)*
operator  →  io_format | if_stmt | ok
io_format →  io:format("строка")
if_stmt   →  if ЦИФРА -> body end
          |  if ЦИФРА -> body ; true -> body end
```

## Распознаваемые лексемы

| Лексема | Токен | ID |
|---------|-------|-----|
| `\n`, `\r\n`, `\t` | NEXT | 0 |
| пробел | SPACE | 1 |
| `-module` | MODULE | 2 |
| `-export` | EXPORT | 3 |
| `(` | OPEN_PAREN | 4 |
| `)` | CLOSE_PAREN | 5 |
| `[` | OPEN_BRACKET | 6 |
| `]` | CLOSE_BRACKET | 7 |
| `/` | SLASH | 8 |
| `0` | ZERO | 9 |
| `main` | MAIN | 10 |
| `->` | ARROW | 11 |
| `.` | DOT | 12 |
| `io:format` | IO_FORMAT | 13 |
| `"..."` | STRING | 14 |
| `,` | COMMA | 15 |
| `if` | IF | 16 |
| цифра 1-9 | DIGIT | 17 |
| `end` | END | 18 |
| `;` | SEMICOLON | 19 |
| `true` | TRUE | 20 |
| `ok` | OK | 21 |

## Как собрать

Выполнить в каталоге проекта:

```bash
make
```

Команда `make`:
- запускает `flex` для генерации `lex.yy.c` из `text.l`;
- компилирует `lex.yy.c` в исполняемый файл `lexer`;
- компилирует `main.c` в исполняемый файл `parser`.

## Как использовать

Программа проверяет синтаксическую корректность кода в файле `test.txt`.

```bash
make run
```

Команда `make run`:
1. Запускает лексер: `./lexer < test.txt > out.txt` — преобразует код в последовательность токенов;
2. Запускает парсер: `./parser` — читает токены из `out.txt` и проверяет грамматику.

**Вывод:**
- `accept` — код соответствует грамматике;
- `reject` — в коде есть синтаксические ошибки.

## Пример корректного кода (`test.txt`)

```erlang
-module(main).
-export([main/0]).

main() ->
    io:format("Hello World"),
    io:format("Second message"),
    if 3 ->
        io:format("condition is true"),
        io:format("more output")
    end,
    if 5 ->
        io:format("simple branch")
    ; true ->
        io:format("else branch"),
        if 1 ->
            io:format("nested if")
        end
    end,
    ok.
```

## Очистка

```bash
make clean
```

Удаляет сгенерированные файлы: `lexer`, `parser`, `lex.yy.c`, `out.txt`.
