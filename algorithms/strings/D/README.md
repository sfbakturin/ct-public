# [D. Быстрый поиск подстроки в строке](D.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Даны строки $p$ и $t$. Требуется найти все вхождения строки $p$ в строку $t$ в качестве подстроки.

## Входные данные

Первая строка входного файла содержит $p$, вторая — $t$ $(1 \leqslant |p|, |t| \leqslant 10^6)$. Строки состоят из букв латинского алфавита.

## Выходные данные

В первой строке выведите количество вхождений строки $p$ в строку $t$. Во второй строке выведите в возрастающем порядке номера символов строки $t$, с которых начинаются вхождения $p$. Символы нумеруются с единицы.

## Примеры

**входные данные**:

```text
aba
abaCaba
```

**выходные данные**:

```text
2
1 5
```