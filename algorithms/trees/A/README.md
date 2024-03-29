# [A. BST или нет?](TaskA.c)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 1 секунда   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Вам дано какое-то двоичное дерево. Проверьте, является ли оно BST.

## Входные данные

В первой строке дано целое число $n$ – количество вершин в дереве $(1 \leqslant n \leqslant 100000)$.

В следующих $n$ строках даны описания вершин. Каждая строка содержит число $x$, содержащееся в вершине, затем два числа $l$ и $r$ – номера левого и правого ребёнка, или $-1$, если ребёнка нет, $(1 \leqslant x \leqslant 10^9, ~ 1 \leqslant l, r \leqslant n)$.

В последней строке дан номер вершины, являющейся корнем.

Вершины нумеруются в порядке, в котором они перечислены во входных данных. Гарантируется, что заданная структура является бинарным деревом.

## Выходные данные

Выведите «`YES`», если данное дерево является BST, иначе «`NO`»

## Примеры

**входные данные**:

```text
3
10 2 3
5 -1 -1
11 -1 -1
1
```

**выходные данные**:

```text
YES
```

**входные данные**:

```text
2
1 2 -1
2 -1 -1
1
```

**выходные данные**:

```text
NO
```
