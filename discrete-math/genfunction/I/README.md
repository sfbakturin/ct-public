# [I. Генератор случайных чисел](I.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Одним из возможных способов написать генератор случайных чисел являются линейные рекурренты.

Рассмотрим следующую линейную рекурренту: $A_i = (A_{i - 1}C_1 + A_{i - 2}C_2 + \ldots + A_{i - k}C_{k})~\mathrm{mod}~104857601$, где $i \geqslant k + 1$.

Вам даны начальные значения $A_1, ~ A_2, ~ \ldots, ~ A_k$, а также коэффициенты рекурренты $C_1, ~ C_2, ~ \ldots, ~ C_k$.

Вычислите $A_n$ для заданного $n$.

## Входные данные

В первой строке дано число $k$ $(1 \leqslant k \leqslant 1000)$, и число $n$ $(1 \leqslant n \leqslant 10^{18})$.

Вторая строка содержит ровно $k$ чисел: $A_1, ~ A_2, ~ \ldots, ~ A_k$ $(0 \leqslant A_i < 104857601)$.

В третьей строке записаны ровно $k$ чисел: $C_1, ~ C_2, ~ \ldots, ~ C_k$ $(0 \leqslant C_i < 104857601)$.

## Выходные данные

Выведите одно число — ответ на задачу.

## Примеры

**входные данные**:

```text
3 5
1 2 3
4 5 6
```

**выходные данные**:

```text
139
```
