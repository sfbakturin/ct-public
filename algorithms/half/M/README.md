# [M. Приближенный двоичный поиск](M.java)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Даны два массива. Первый массив отсортирован по не-убыванию, второй массив содержит запросы — целые числа.

Для каждого запроса выведите число из первого массива наиболее близкое (то есть с минимальным модулем разности) к числу в этом запросе . Если таких несколько, выведите меньшее из них.

## Входные данные

В первой строке входных данных содержатся числа $n$ и $k$ $(0 < n, k \leqslant 10^{5})$. Во второй строке задаются $n$ чисел первого массива, отсортированного по не-убыванию, а в третьей строке — $k$ чисел второго массива. Каждое число в обоих массивах по модулю не превосходит $2 \cdot 10^{9}$.

## Выходные данные

Для каждого из $k$ чисел выведите в отдельную строку число из первого массива, наиболее близкое к данному. Если таких несколько, выведите меньшее из них.

## Примеры

**входные данные**:

```text
5 5
1 3 5 7 9
2 4 8 1 6
```

**выходные данные**:

```text
1
3
7
1
5
```