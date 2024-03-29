# [F. Подсчет деревьев](F.c)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Бинарным деревом в этой задаче назовем дерево, каждая вершина которого имеет выделенное левое и выделенное правое поддерево, каждое из которых может быть пустым (в этом случае вершина является листом).

Заданы числа $c_1, ~ c_2, ~ \ldots, ~ c_k$. Посчитайте количество различных бинарных деревьев, в которых каждая вершина может иметь вес, равный любому из значений $c_i$. Вершины равного веса считаются одинаковыми.

## Входные данные

В первой строке содержатся два целых числа $k$ и $m$ $(1 \leqslant k, m \leqslant 2000)$ — количество весов вершин и максимальный вес дерева. В следующей строке содержатся числа $c_i$ $(1 \leqslant c_i \leqslant m)$. Все $c_i$ различны.

## Выходные данные

Выведите $m$ чисел — количество деревьев веса $1, ~ 2, ~ \ldots, ~ m$ по модулю $10^9 + 7$.

## Примеры

**входные данные**:

```text
2 5
1 3
```

**выходные данные**:

```text
1 2 6 18 57
```

**входные данные**:

```text
1 10
2
```

**выходные данные**:

```text
0 1 0 2 0 5 0 14 0 42
```
