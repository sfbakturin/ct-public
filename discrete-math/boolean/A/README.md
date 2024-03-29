# [A. Бинарные отношение](A.java)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Дано число $n$ и два бинарных отношения на множестве размера $n$. Для каждого из этих отношений определите, являются ли они рефлексивными, антирефлексивными, симметричными, антисимметричными и транзитивными, а так же найдите их композицию.

## Входные данные

В первой строке содержится число $n$ — размер носителя $(1 \leqslant n \leqslant 100)$. В следующих $n$ строках находится по $n$ чисел — описание первого отношения. Если $j$-е число $i$-й строки равно $1$, то пара $(i, j)$ лежит в отношении, иначе эта пара не лежит в отношении. В следующих $n$ строках находится описание второго отношения в таком же формате.

## Выходные данные

Для каждого из пяти свойств из условия выведите в первой строке $1$, если первое отношение обладает этим свойством, и $0$ иначе. Во второй строке выведите описание второго отношения в таком же формате.

В следующих $n$ строках выведите по $n$ чисел — композицию двух отношений в таком же формате, что и во входных данных.

## Примеры

**входные данные**:

```text
3
0 1 0
0 0 1
1 0 0
1 1 0
0 1 1
1 0 1
```

**выходные данные**:

```text
0 1 0 1 0
1 0 0 1 0
0 1 1
1 0 1
1 1 0
```
