# `optional`

В этом задании вам необходимо написать [`std::optional`](https://en.cppreference.com/w/cpp/utility/optional). Интерфейс вы найдете в репозитории, в файле [`optional.h`](optional.h). Поведение должно соответствовать стандарту.

Обратите внимание, что должно выполняться:

* если у типа `T` нет какого-то из *special members* (например, `std::is_copy_constructible<T> == false`), то это свойство должно сохраняться и для `optional<T>`.
* если тип `T` удовлетворяет какому-то из *traits* `std::is_trivially_*`, то и `optional<T>` должен ему удовлетворять.
* `optional` может использоваться в `constexpr` контексте, см. тесты со `static_assert`. Исключение - для нетривиально присваиваемых классов не требуется реализация `constexpr operator=`.
