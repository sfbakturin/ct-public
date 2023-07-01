# [B. Число минимумов на отрезке](TaskB.java)

| Ограничения                                   |
|:---------------------------------------------:|
| ограничение по времени на тест: 1 секунда     |
| ограничение по памяти на тест: 1024 мегабайта |
| входной файл: `стандартный ввод`              |
| выходной файл: `стандартный вывод`            |

## Условие

Теперь измените код дерева отрезков, чтобы кроме минимума на отрезке считалось также и число элементов, равных минимуму.

## Входные данные

Первая строка содержит два числа $n$ и $m$ $(1 \leqslant n, m \leqslant 100000)$ — размер массива и число операций. Следующая строка содержит $n$ чисел $a_i$ — начальное состояние массива $(0 \leqslant a_i \leqslant 10^9)$. Далее следует описание операций. Описание каждой операции имеет следующий вид:

* $1~i~v$ — присвоить элементу с индексом $i$ значение $v$ $(0 \leqslant i < n, ~ 0 \leqslant v \leqslant 10^9)$.
* $2~l~r$ — найти минимум и число элементов, равных минимуму, среди элементов с индексами от $l$ до $r - 1$ $(0 \leqslant l < r \leqslant n)$.

## Выходные данные

Для каждой операции второго типа выведите два числа — минимум на заданном отрезке и число элементов, равных этому минимуму.

## Примеры

**входные данные**:

```text
5 5
3 4 3 5 2
2 0 3
1 1 2
2 0 3
1 0 2
2 0 5
```

**выходные данные**:

```text
3 2
2 1
2 3
```