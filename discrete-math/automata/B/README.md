# [B. Слово и НКА](B.java)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 2 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `problem2.in`                 |
| выходной файл: `problem2.out`               |

## Условие

Задан недетерминированный конечный автомат и слово. Определить, допускает ли данный НКА заданное слово.

## Входные данные

В первой строке входного файла находится слово, состоящее из не более чем 10000 строчных латинских букв.

Во второй строке содержатся числа $n$, $m$ и $k$ — количество состояний, переходов и допускающих состояний в автомате соответственно $(1 \leqslant n \leqslant 100, ~ 1 \leqslant m \leqslant 1000, ~ 1 \leqslant k \leqslant n)$.

В следующей строке содержатся $k$ чисел — номера допускающих состояний (состояния пронумерованы от $1$ до $n$).

В следующих $m$ строках описываются переходы в формате “*a b c*”, где *a* — номер исходного состояния перехода, *b* — номер состояния, в которое осуществляется переход и *c* — символ (строчная латинская буква), по которому осуществляется переход.

Стартовое состояние автомата всегда имеет номер $1$.

## Выходные данные

Требуется выдать строку “`Accepts`”, если автомат принимает заданное слово, или “`Rejects`” в противном случае.

## Примеры

**входные данные**:

```text
abacaba
4 6 1
2
1 2 a
2 1 c
2 3 b
3 2 a
2 4 b
1 4 a
```

**выходные данные**:

```text
Accepts
```
