#pragma once

#include <iterator>

#include "bimap_element.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace bimap_details {
template <typename Key, typename Value, bool Tree>
struct base_iterator {
public:
  using iterator_category = std::bidirectional_iterator_tag;
  using value_type = std::conditional_t<Tree, Key, Value>;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type*;
  using reference = value_type&;

private:
  using value_type_another = std::conditional_t<Tree, Value, Key>;
  using base_t = element_base;
  using elem_t = element_value<Tree, value_type>;
  using elem_another_t = element_value<!Tree, value_type_another>;
  using doub_t = element_data<Key, Value>;

  base_t* value = nullptr;

  template <typename FLt, typename FRt, typename FCLt, typename FCRt>
  friend struct ::bimap;

  friend struct base_iterator<Key, Value, !Tree>;

  base_iterator(base_t* value) : value(value) {}

public:
  // Элемент на который сейчас ссылается итератор.
  // Разыменование итератора end_left() неопределено.
  // Разыменование невалидного итератора неопределено.
  value_type const& operator*() const {
    return static_cast<elem_t*>(value)->get();
  }
  value_type const* operator->() const {
    return static_cast<pointer>(&static_cast<elem_t*>(value)->get());
  }

  // Переход к следующему по величине left'у.
  // Инкремент итератора end_left() неопределен.
  // Инкремент невалидного итератора неопределен.
  base_iterator& operator++() {
    value = value->next(value);
    return *this;
  }
  base_iterator operator++(int) {
    base_iterator res(*this);
    ++(*this);
    return res;
  }

  // Переход к предыдущему по величине left'у.
  // Декремент итератора begin_left() неопределен.
  // Декремент невалидного итератора неопределен.
  base_iterator& operator--() {
    value = value->prev(value);
    return *this;
  }
  base_iterator operator--(int) {
    base_iterator res(*this);
    --(*this);
    return res;
  }

  // left_iterator ссылается на левый элемент некоторой пары.
  // Эта функция возвращает итератор на правый элемент той же пары.
  // end_left().flip() возращает end_right().
  // end_right().flip() возвращает end_left().
  // flip() невалидного итератора неопределен.
  base_iterator<Key, Value, !Tree> flip() const {
    if (value->parent) {
      return base_iterator<Key, Value, !Tree>(
          static_cast<base_t*>(static_cast<elem_another_t*>(
              static_cast<doub_t*>(static_cast<elem_t*>(value)))));
    } else {
      return base_iterator<Key, Value, !Tree>(value->right);
    }
  }

  bool operator==(base_iterator const& other) const noexcept {
    return value == other.value;
  }
  bool operator!=(base_iterator const& other) const noexcept {
    return value != other.value;
  }
};
} // namespace bimap_details
