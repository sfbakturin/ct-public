# [H. Скобки](H.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Требуется определить, является ли правильной данная последовательность круглых, квадратных и фигурных скобок.

## Входные данные

В единственной строке входного файла записано подряд $N$ $(1 \leqslant N \leqslant 10^{5})$.

## Выходные данные

В выходной файл вывести `YES`, если данная последовательность является правильной, и `NO` в противном случае.

## Примеры

**входные данные**:

```text
()
```

**выходные данные**:

```text
YES
```

**входные данные**:

```text
([]){}
```

**выходные данные**:

```text
YES
```

**входные данные**:

```text
[]([)]
```

**выходные данные**:

```text
NO
```

## Примечание

Скобочная последовательность называется правильной, если ее можно получить из какого-либо математического выражения вычеркиванием всех символов, кроме скобок.

Формальное определение правильной скобочной последовательности таково:

* Пустая последовательность является правильной.
* Если `A` — правильная скобочная последовательность, то `(A)`, `[A]` и `{A}` — правильные скобочные последовательности.
* Если `A` и `B` — правильные скобочные последовательности, то `AB` — правильная скобочная последовательность.