# [E. Компоненты вершинной двусвязности](E.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 64 мегабайта |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Компонентой вершинной двусвязности графа $\langle V, E \rangle$ называется максимальный по включению подграф (состоящий из вершин и ребер), такой что любые два ребра из него лежат на вершинно простом цикле.

Дан неориентированный граф без петель. Требуется выделить компоненты вершинной двусвязности в нем.

## Входные данные

Первая строка входного файла содержит два натуральных числа $n$ и $m$ — количества вершин и рёбер графа соответственно $(1 \leqslant n \leqslant 20000, ~ 1 \leqslant m \leqslant 200000)$.

Следующие $m$ строк содержат описание рёбер по одному на строке. Ребро номер $i$ описывается двумя натуральными числами $b_i$, $e_i$ — номерами концов ребра $(1 \leqslant b_i, e_i \leqslant n)$.

## Выходные данные

В первой строке выходного файла выведите целое число $k$ — количество компонент вершинной двусвязности графа. Во второй строке выведите $m$ натуральных чисел $a_1, ~ a_2, ~ \ldots, ~ a_m$, не превосходящих $k$, где $a_i$ — номер компоненты вершинной двусвязности, которой принадлежит $i$-е ребро. Ребра нумеруются с единицы в том порядке, в котором они заданы во входном файле.

## Примеры

**входные данные**:

```text
5 6
1 2
2 3
3 1
1 4
4 5
5 1
```

**выходные данные**:

```text
2
1 1 1 2 2 2 
```
