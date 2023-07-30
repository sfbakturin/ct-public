# [B. Поиск гамильтонова цикла в условиях теоремы Хватала](B.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 1 секунда   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Дан граф из $N$ вершин, для которого выполняется условие теоремы Хватала, то есть, в отсортированной последовательности его степеней вершин $d_k$ для любого $k < \dfrac{n}{2}$ верно либо $d_k > k$, либо $d_{n - k} \geqslant n - k$. Ваша задача — найти гамильтонов цикл.

## Входные данные

На первой строке входных данных записано целое число $N$ $(3 \leqslant N \leqslant 100)$ — количество вершин в графе. На следующих $N$ строках записана матрица смежности. Так как матрица смежности симметрична, а на диагонали всегда стоят нули, на $i$-й строке записаны $i - 1$ символ — нули и единицы. Если $j$-й символ $i$-й строки равен единице, значит есть ребро между вершинами $i$ и $j$.

## Выходные данные

Выведите перестановку из $N$ чисел — номера вершин в порядке гамильтонова цикла.

## Примеры

**входные данные**:

```text
4

1
11
101
```

**выходные данные**:

```text
1 2 3 4
```