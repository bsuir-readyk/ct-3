# Примеры ввода-вывода для задания 1

## Режим 1: Проверка корректности строки

### Корректные пути

```bash
$
./main 1 "C:\Windows\winmine.exe"
C:\Windows\winmine.exe - Строка является лексемой

$
./main 1 "D:\WebServer\home\site.by\www\.htaccess"
D:\WebServer\home\site.by\www\.htaccess - Строка является лексемой

$
./main 1 "Z:\autoexec.bat"
Z:\autoexec.bat - Строка является лексемой

$
./main 1 "N:\testfile."
N:\testfile. - Строка является лексемой

$
./main 1 "X:\testfile2"
X:\testfile2 - Строка является лексемой

$
./main 1 "A:\"
A:\ - Строка является лексемой

$
./main 1 "B:\folder\file.txt"
B:\folder\file.txt - Строка является лексемой

$
./main 1 "C:\Program Files\App\app.exe"
C:\Program Files\App\app.exe - Строка является лексемой
```

### Некорректные пути

```bash
$
./main 1 "C:Windows\winmine.exe"
C:Windows\winmine.exe - Строка не является лексемой

$
./main 1 "c:\Windows\winmine.exe"
c:\Windows\winmine.exe - Строка не является лексемой

$
./main 1 "C:/Windows/winmine.exe"
C:/Windows/winmine.exe - Строка не является лексемой

$
./main 1 "C:\Windows\winmine*.exe"
C:\Windows\winmine*.exe - Строка не является лексемой

$
./main 1 "C:\Windows\winmine?.exe"
C:\Windows\winmine?.exe - Строка не является лексемой

$
./main 1 "C:\Windows\winmine|.exe"
C:\Windows\winmine|.exe - Строка не является лексемой

$
./main 1 "C:\Windows\winmine<.exe"
C:\Windows\winmine<.exe - Строка не является лексемой

$
./main 1 "C:\Windows\winmine>.exe"
C:\Windows\winmine>.exe - Строка не является лексемой

$
./main 1 "C:\Windows\winmine\".exe"
C:\Windows\winmine\".exe - Строка не является лексемой

$
./main 1 ""
 - Строка не является лексемой

$
./main 1 "C"
C - Строка не является лексемой

$
./main 1 "C:"
C: - Строка не является лексемой
```

## Режим 2: Поиск всех подстрок, соответствующих требованиям

### Пример 1: Простая строка с одним путём

```bash
$
./main 2 "C:\Windows\winmine.exe"
Лексемы:
C:\Windows\winmine.exe
C:\Windows\winmine
C:\Windows
C:\
```

### Пример 2: Строка с несколькими путями

```bash
$
./main 2 "C:\Windows\winmine.exe and D:\Program Files\app.exe"
Лексемы:
C:\Windows\winmine.exe
C:\Windows\winmine
C:\Windows
C:\
D:\Program
D:\
```

### Пример 3: Путь с точкой в конце

```bash
$
./main 2 "N:\testfile."
Лексемы:
N:\testfile.
N:\testfile
N:\
```

### Пример 4: Путь с подчёркиванием и дефисом

```bash
$
./main 2 "D:\WebServer\home\site.by\www\.htaccess"
Лексемы:
D:\WebServer\home\site.by\www\.htaccess
D:\WebServer\home\site.by\www
D:\WebServer\home\site.by
D:\WebServer\home\site
D:\WebServer\home
D:\WebServer
D:\
```

### Пример 5: Текст с путём в середине

```bash
$
./main 2 "The file is at Z:\autoexec.bat and it works"
Лексемы:
Z:\autoexec.bat
Z:\autoexec
Z:\
```

### Пример 6: Путь с цифрами

```bash
$
./main 2 "C:\folder123\file456.txt"
Лексемы:
C:\folder123\file456.txt
C:\folder123\file456
C:\folder123
C:\
```

### Пример 7: Путь с различными допустимыми символами

```bash
$
./main 2 "D:\test-folder_name.file123"
Лексемы:
D:\test-folder_name.file123
D:\test-folder_name.file
D:\test-folder_name
D:\test-folder
D:\test
D:\
```

### Пример 8: Строка без корректных путей

```bash
$
./main 2 "This is just a text without paths"
Лексемы:

```

### Пример 9: Путь с недопустимыми символами (пробел прерывает путь)

```bash
$
./main 2 "C:\Program Files\app.exe"
Лексемы:
C:\Program
C:\
```

### Пример 10: Несколько путей подряд

```bash
$
./main 2 "C:\file1.txtD:\file2.txt"
Лексемы:
C:\file1.txt
C:\file1
C:\
D:\file2.txt
D:\file2
D:\
```

