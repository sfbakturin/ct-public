# [D. Компоненты реберной двусвязности](D.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 64 мегабайта |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Компонентой реберной двусвязности графа $\langle V, E \rangle$ называется подмножество вершин $S \subset V$ такое что для любых различных $u$ и $v$ из этого множества существует не менее двух реберно не пересекающихся путей из $u$ в $v$.

Дан неориентированный граф. Требуется выделить компоненты реберной двусвязности в нем.

## Входные данные

Первая строка входного файла содержит два натуральных числа $n$ и $m$ — количества вершин и рёбер графа соответственно $(1 \leqslant n \leqslant 20000, ~ 1 \leqslant m \leqslant 200000)$.

Следующие $m$ строк содержат описание рёбер по одному на строке. Ребро номер $i$ описывается двумя натуральными числами $b_i$, $e_i$ — номерами концов ребра $(1 \leqslant b_i, e_i \leqslant n)$.

## Выходные данные

В первой строке выходного файла выведите целое число $k$ — количество компонент реберной двусвязности графа. Во второй строке выведите $n$ натуральных чисел $a_1, ~ a_2, ~ \ldots, ~ a_n$, не превосходящих $k$, где $a_i$ — номер компоненты реберной двусвязности, которой принадлежит $i$-я вершина.

## Примеры

**входные данные**:

```text
6 7
1 2
2 3
3 1
1 4
4 5
4 6
5 6
```

**выходные данные**:

```text
2
1 1 1 2 2 2
```