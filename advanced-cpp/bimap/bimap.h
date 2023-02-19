#pragma once

#include <cstddef>
#include <memory>
#include <stdexcept>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template <typename Lt, typename Rt, typename CLt, typename CRt>
struct bimap;

#include "bimap_element.h"
#include "bimap_iterator.h"
#include "bimap_tree.h"

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
struct bimap {
public:
  using left_t = Left;
  using right_t = Right;

  using left_iterator = bimap_details::base_iterator<left_t, right_t, true>;
  using right_iterator = bimap_details::base_iterator<left_t, right_t, false>;

private:
  using base_t = bimap_details::element_base;
  using data_t = bimap_details::element_data<left_t, right_t>;
  using value_left_t = bimap_details::element_value<true, left_t>;
  using value_right_t = bimap_details::element_value<false, right_t>;

  std::size_t count = 0;
  bimap_details::tree<left_t, true, CompareLeft> left_tree;
  bimap_details::tree<right_t, false, CompareRight> right_tree;

  template <typename left_t_f = left_t, typename right_t_f = right_t>
  left_iterator insert_impl(left_t_f&& left, right_t_f&& right) {
    if (find_left(left) != end_left() || find_right(right) != end_right()) {
      return end_left();
    }
    count++;
    auto* elem = new data_t(std::forward<left_t_f>(left),
                            std::forward<right_t_f>(right));
    auto* inserted = left_tree.insert(
        static_cast<base_t*>(static_cast<value_left_t*>(elem)));
    right_tree.insert(static_cast<base_t*>(static_cast<value_right_t*>(elem)));
    return left_iterator(inserted);
  }

  void erase_impl(base_t* left_to_delete, base_t* right_to_delete) {
    count--;
    left_tree.erase(left_to_delete);
    right_tree.erase(right_to_delete);
    delete static_cast<data_t*>(static_cast<value_left_t*>(left_to_delete));
  }

  void clear() {
    erase_left(begin_left(), end_left());
  }

public:
  void swap(bimap& other) {
    std::swap(count, other.count);
    left_tree.swap(other.left_tree);
    right_tree.swap(other.right_tree);
  }

  // Создает bimap не содержащий ни одной пары.
  bimap(CompareLeft compare_left = CompareLeft(),
        CompareRight compare_right = CompareRight())
      : left_tree(std::move(compare_left)),
        right_tree(std::move(compare_right)) {
    left_tree.set_another_tree(right_tree.end());
    right_tree.set_another_tree(left_tree.end());
  }

  // Конструкторы от других и присваивания.
  bimap(bimap const& other)
      : left_tree(other.left_tree.get_comparator()),
        right_tree(other.right_tree.get_comparator()) {
    left_tree.set_another_tree(right_tree.end());
    right_tree.set_another_tree(left_tree.end());
    try {
      for (left_iterator it = other.begin_left(); it != other.end_left();
           it++) {
        insert(*it, *(it.flip()));
      }
    } catch (...) {
      clear();
      throw;
    }
  }
  bimap(bimap&& other) noexcept
      : left_tree(std::move(other.left_tree)),
        right_tree(std::move(other.right_tree)), count(std::move(other.count)) {
  }

  bimap& operator=(bimap const& other) {
    if (this == &other) {
      return *this;
    }
    bimap(other).swap(*this);
    return *this;
  }
  bimap& operator=(bimap&& other) noexcept {
    if (this == &other) {
      return *this;
    }
    bimap(std::move(other)).swap(*this);
    return *this;
  }

  // Деструктор. Вызывается при удалении объектов bimap.
  // Инвалидирует все итераторы ссылающиеся на элементы этого bimap
  // (включая итераторы ссылающиеся на элементы следующие за последними).
  ~bimap() {
    clear();
  }

  // Вставка пары (left, right), возвращает итератор на left.
  // Если такой left или такой right уже присутствуют в bimap, вставка не
  // производится и возвращается end_left().
  left_iterator insert(left_t const& left, right_t const& right) {
    return insert_impl(left, right);
  }
  left_iterator insert(left_t const& left, right_t&& right) {
    return insert_impl(left, std::move(right));
  }
  left_iterator insert(left_t&& left, right_t const& right) {
    return insert_impl(std::move(left), right);
  }
  left_iterator insert(left_t&& left, right_t&& right) {
    return insert_impl(std::move(left), std::move(right));
  }

  // Удаляет элемент и соответствующий ему парный.
  // erase невалидного итератора неопределен.
  // erase(end_left()) и erase(end_right()) неопределены.
  // Пусть it ссылается на некоторый элемент e.
  // erase инвалидирует все итераторы ссылающиеся на e и на элемент парный к e.
  left_iterator erase_left(left_iterator it) {
    left_iterator res(std::next(it));
    erase_impl(it.value, it.flip().value);
    return res;
  }
  // Аналогично erase, но по ключу, удаляет элемент если он присутствует, иначе
  // не делает ничего Возвращает была ли пара удалена.
  bool erase_left(left_t const& left) {
    left_iterator found = find_left(left);
    if (found != end_left()) {
      erase_left(found);
      return true;
    } else {
      return false;
    }
  }

  right_iterator erase_right(right_iterator it) {
    right_iterator res(std::next(it));
    erase_impl(it.flip().value, it.value);
    return res;
  }
  bool erase_right(right_t const& right) {
    right_iterator found = find_right(right);
    if (found != end_right()) {
      erase_right(found);
      return true;
    } else {
      return false;
    }
  }

  // erase от ренжа, удаляет [first, last), возвращает итератор на последний
  // элемент за удаленной последовательностью.
  left_iterator erase_left(left_iterator first, left_iterator last) {
    for (left_iterator it(first); it != last; it = erase_left(it)) {}
    return last;
  }
  right_iterator erase_right(right_iterator first, right_iterator last) {
    for (right_iterator it(first); it != last; it = erase_right(it)) {}
    return last;
  }

  // Возвращает итератор по элементу. Если не найден - соответствующий end().
  left_iterator find_left(left_t const& left) const {
    base_t* found = left_tree.find(left);
    if (!found) {
      return end_left();
    } else {
      return left_iterator(found);
    }
  }
  right_iterator find_right(right_t const& right) const {
    base_t* found = right_tree.find(right);
    if (!found) {
      return end_right();
    } else {
      return right_iterator(found);
    }
  }

  // Возвращает противоположный элемент по элементу.
  // Если элемента не существует -- бросает std::out_of_range.
  right_t const& at_left(left_t const& key) const {
    left_iterator found = find_left(key);
    if (found != end_left()) {
      return *found.flip();
    } else {
      throw std::out_of_range("No such element was found!");
    }
  }
  left_t const& at_right(right_t const& key) const {
    right_iterator found = find_right(key);
    if (found != end_right()) {
      return *found.flip();
    } else {
      throw std::out_of_range("No such element was found!");
    }
  }

  // Возвращает противоположный элемент по элементу.
  // Если элемента не существует, добавляет его в bimap и на противоположную.
  // сторону кладет дефолтный элемент, ссылку на который и возвращает.
  // Если дефолтный элемент уже лежит в противоположной паре - должен поменять
  // соответствующий ему элемент на запрашиваемый (смотри тесты).
  template <typename = std::is_default_constructible<Right>>
  right_t const& at_left_or_default(left_t const& key) {
    left_iterator found_left = find_left(key);
    if (found_left != end_left()) {
      return *found_left.flip();
    }
    right_t default_element{};
    right_iterator found_right = find_right(default_element);
    if (found_right != end_right()) {
      erase_right(found_right);
    }
    return *(insert(key, std::move(default_element)).flip());
  }
  template <typename = std::is_default_constructible<Left>>
  left_t const& at_right_or_default(right_t const& key) {
    right_iterator found_right = find_right(key);
    if (found_right != end_right()) {
      return *found_right.flip();
    }
    left_t default_element{};
    left_iterator found_left = find_left(default_element);
    if (found_left != end_left()) {
      erase_left(found_left);
    }
    return *insert(std::move(default_element), key);
  }

  // lower и upper bound'ы по каждой стороне.
  // Возвращают итераторы на соответствующие элементы.
  // Смотри std::lower_bound, std::upper_bound.
  left_iterator lower_bound_left(const left_t& left) const {
    return left_iterator(left_tree.next(left));
  }
  left_iterator upper_bound_left(const left_t& left) const {
    return left_iterator(left_tree.prev(left));
  }

  right_iterator lower_bound_right(const right_t& right) const {
    return right_iterator(right_tree.next(right));
  }
  right_iterator upper_bound_right(const right_t& right) const {
    return right_iterator(right_tree.prev(right));
  }

  // Возвращает итератор на минимальный по порядку left.
  left_iterator begin_left() const {
    return left_iterator(left_tree.begin());
  }

  // Возвращает итератор на максимальный по порядку left.
  left_iterator end_left() const {
    return left_iterator(left_tree.end());
  }

  // Возвращает итератор на минимальный по порядку right.
  right_iterator begin_right() const {
    return right_iterator(right_tree.begin());
  }

  // Возвращает итератор на максимальный по порядку right.
  right_iterator end_right() const {
    return right_iterator(right_tree.end());
  }

  // Проверка на пустоту.
  bool empty() const {
    return !count;
  }

  // Возвращает размер бимапы (кол-во пар).
  std::size_t size() const {
    return count;
  }

  // Операторы сравнения.
  friend bool operator==(bimap const& a, bimap const& b) {
    if (a.count != b.count) {
      return false;
    }
    for (auto it_a = a.begin_left(), it_b = b.begin_left();
         it_a != a.end_left(); it_a++, it_b++) {
      if (!a.left_tree.is_equals(*it_a, *it_b) ||
          !a.right_tree.is_equals(*(it_a.flip()), *(it_b.flip()))) {
        return false;
      }
    }
    return true;
  }
  friend bool operator!=(bimap const& a, bimap const& b) {
    return !(a == b);
  }
};
