# [F. Лошадью ходи](F.java)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 0.5 секунд  |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `knight.in`                   |
| выходной файл: `knight.out`                 |

## Условие

В левом верхнем углу прямоугольной доски $N \times M$ находится шахматный конь. Перемещаться по доске он может только двумя способами: на две клетки вправо и на одну вниз, либо на две клетки вниз и на одну вправо.

![alt text](horse1.png)

Ваша задача состоит в том, чтобы посчитать количество способов, которыми конь может попасть в правую нижнюю клетку.

## Входные данные

В единственной строчке находятся два числа $N$ и $M$ $(1 \leqslant N, M \leqslant 50)$ — размеры доски.

## Выходные данные

Нужно вывести одно число — количество различных способов переместиться коню из левой верхней в правую нижнюю клетку.

## Примеры

**входные данные**:

```text
4 4
```

**выходные данные**:

```text
2
```
