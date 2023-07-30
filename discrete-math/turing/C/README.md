# [C. Зеркальное отображение](mirror.out)

| Ограничения                                    |
|:----------------------------------------------:|
| ограничение по времени на тест: 10000000 шагов |
| ограничение по памяти на тест: 500 состояний   |
| входной файл: неизвестно                       |
| выходной файл: `mirror.out`                    |

## Условие

На ленте записано слово $w$. Требуется найти слово $s = ww;$, где $w'$ - слово w, записанное в обратном порядке. Реализуйте алгоритм на одноленточной машине Тьюринга.

## Входные данные

На ленте записано слово $w$ $(w \in \{0, 1\}*, ~ 1 \leqslant |w| \leqslant 200)$. Головка указывает на первую букву слова.

## Выходные данные

В конце на ленте должно быть записано слово s, головка должна указывать на начало этого слова. На ленте ничего не должно быть. Слово должно быть допущено.

## Примеры

**лента в начале**:

```text
1 0 1 0 0
```

**лента в конце**: ***accepted*** - **`1 0 1 0 0 0 0 1 0 1`**