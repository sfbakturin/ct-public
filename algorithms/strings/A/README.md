# [A. Сравнения подстрок](A.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Дана строка $s$. Ответьте на $m$ запросов вида: равны ли подстроки $s[a..b]$ и $s[c..d]$.

## Входные данные

В первой строке ввода записана строка $s$ $(1 \leqslant |s| \leqslant 10^5)$.

Во второй строке записано целое число $m$ — количество запросов $(1 \leqslant m \leqslant 10^5)$.

В следующих $m$ строках четверки чисел $a$, $b$, $c$, $d$ $(1 \leqslant a \leqslant b \leqslant |s|, ~ 1 \leqslant c \leqslant d \leqslant |s|)$.

## Выходные данные

Выведите $m$ строк. Выведите `Yes`, если подстроки совпадают, и `No` иначе.

## Примеры

**входные данные**:

```text
trololo
3
1 7 1 7
3 5 5 7
1 1 1 5
```

**выходные данные**:

```text
Yes
Yes
No
```