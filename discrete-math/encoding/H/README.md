# [H. Арифметическое декодирование](H.py)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Дан результат применения арифметического кодирования к некоторой строке $s$, состоящей из маленьких букв английского алфавита. Декодируйте и восстановите исходную строку.

## Входные данные

В первой строке содержится число $n$ $(1 \leqslant n \leqslant 26)$, обозначающее, что в исходной строке присутствовали только первые $n$ букв английского алфавита. Во второй строке содержатся $n$ чисел $c_a, ~ c_b, ~ \ldots$ — количество раз, которое в строке встречается каждая из $n$ букв $(1 \leqslant \sum\limits{c_i} \leqslant 100)$. Во третьей строке содержится строка из $q$ $(1 \leqslant q \leqslant 1000)$. нулей и единиц — двоичное представление числителя $p$ дроби $\dfrac{p}{2^q}$, возможно, с ведущими нулями.

## Выходные данные

Выведите одну строку длины $\sum\limits{c_i}$ — результат декодирования.

## Примеры

**входные данные**:

```text
3
4 2 1
0110100101
```

**выходные данные**:

```text
abacaba
```

**входные данные**:

```text
2
0 3
0
```

**выходные данные**:

```text
bbb
```
