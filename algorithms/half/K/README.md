# [K. Минимум в очереди](K.cpp)

| Ограничения                                 |
|:-------------------------------------------:|
| ограничение по времени на тест: 4 секунды   |
| ограничение по памяти на тест: 256 мегабайт |
| входной файл: `стандартный ввод`            |
| выходной файл: `стандартный вывод`          |

## Условие

Изначально очередь пуста. Приходят запросы

* `+ x` добавить в конец очереди элемент $x$
* `-` удалить из начала очереди элемент

Гарантируется, что вторая операция не пытается удалить элемент из пустой очереди. После каждой операции нужно выводить минимум в очереди после него.

Ограничение по времени выставлено так, чтобы решение с `std::set` не проходило.

## Входные данные

В первой строке записано единственное число $q$ $(1 \leqslant q \leqslant 10^{6})$ — количество запросов. В следующих $q$ строках записаны сами запросы в описанном выше формате. Все числа во входном файле целые, положительные и не превышают $10^{9}$.

## Выходные данные

Для каждого запроса выведите единственное число — минимум в очереди после выполнения этого запроса. Если после запроса очередь пуста, выводите $-1$.

## Примеры

**входные данные**:

```text
10
+ 1
+ 2
+ 3
+ 4
+ 5
-
-
-
-
-
```

**выходные данные**:

```text
1
1
1
1
1
2
3
4
5
-1
```