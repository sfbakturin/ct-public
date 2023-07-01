# [G. Присваивание, прибавление и сумма](TaskG.java)

| Ограничения                                   |
|:---------------------------------------------:|
| ограничение по времени на тест: 1 секунда     |
| ограничение по памяти на тест: 1024 мегабайта |
| входной файл: `стандартный ввод`              |
| выходной файл: `стандартный вывод`            |

## Условие

Есть массив из $n$ элементов, изначально заполненный нулями. Вам нужно написать структуру данных, которая обрабатывает два вида запросов:

* присвоить всем элементам на отрезке от $l$ до $r - 1$ значение $v$,
* прибавить ко всем элементам на отрезке от $l$ до $r - 1$ число $v$,
* узнать сумму на отрезке от $l$ до $r - 1$.

## Входные данные

Первая строка содержит два числа $n$ и $m$ $(1 \leqslant n, m \leqslant 100000)$ — размер массива и число операций. Далее следует описание операций. Описание каждой операции имеет следующий вид:

* $1~l~r~v$ — присвоить всем элементам на отрезке от $l$ до $r - 1$ значение $v$ $(0 \leqslant l < r \leqslant n, ~ 0 \leqslant v \leqslant 10^9)$.
* $2~l~r~v$ — прибавить ко всем элементам на отрезке от $l$ до $r - 1$ число $v$ $(0 \leqslant l < r \leqslant n, ~ 0 \leqslant v \leqslant 10^9)$.
* $3~l~r$ — узнать сумму на отрезке от $l$ до $r - 1$ $(0 \leqslant l < r \leqslant n)$.

## Выходные данные

Для каждой операции третьего типа выведите соответствующее значение.

## Примеры

**входные данные**:

```text
5 7
1 0 3 3
2 2 4 2
3 1 3
2 1 5 1
1 0 2 2
3 0 3
3 3 5
```

**выходные данные**:

```text
8
10
4
```