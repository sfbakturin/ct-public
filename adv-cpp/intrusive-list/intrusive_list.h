#pragma once

#include <iterator>
#include <type_traits>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace intrusive {
struct default_tag;

template <typename Tag = default_tag>
struct list_element;

template <typename T, typename Tag = default_tag>
struct list;

struct list_element_base {
private:
  list_element_base* next = nullptr;
  list_element_base* prev = nullptr;

  template <typename T, typename Tag>
  friend struct list;

  void transform(list_element_base&& other) noexcept {
    if (other.prev) {
      other.prev->next = this;
    }
    if (other.next) {
      other.next->prev = this;
    }
    next = other.next;
    prev = other.prev;
    other.next = other.prev = nullptr;
  }

public:
  list_element_base() = default;
  list_element_base(list_element_base const&) {}
  list_element_base(list_element_base&& other) {
    transform(std::move(other));
  }

  list_element_base& operator=(list_element_base&& other) {
    transform(std::move(other));
    return *this;
  }

  void unlink() noexcept {
    if (prev) {
      prev->next = next;
    }
    if (next) {
      next->prev = prev;
    }
    next = prev = nullptr;
  }

  ~list_element_base() noexcept {
    unlink();
  }
};

template <typename Tag>
struct list_element : public list_element_base {};

template <typename T, typename Tag>
struct list {
private:
  using tref = T&;
  using tcref = T const&;
  using obj = list_element<Tag>;
  using obj_ptr = obj*;
  using obj_ref = obj&;

  template <typename Type>
  static obj_ptr to_ptr(Type* ptr) {
    return static_cast<obj_ptr>(ptr);
  }

  template <typename Type>
  tref to_ref(Type& ref) const {
    return static_cast<tref>(static_cast<obj_ref>(ref));
  }

  template <bool is_const>
  struct list_iterator {
  private:
    using iterator_element = obj;
    using iterator_pointer = iterator_element*;
    using iterator_pointer_const = iterator_element const*;
    using value_clear = T;
    using iterator_class = list_iterator<is_const>;

  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type =
        std::conditional_t<is_const, value_clear const, value_clear>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

  private:
    friend struct list;

    iterator_pointer value = nullptr;

    explicit list_iterator(iterator_pointer value) : value(value) {}
    explicit list_iterator(iterator_pointer_const value_const)
        : value(const_cast<iterator_pointer>(value_const)) {}

  public:
    list_iterator() = default;

    template <bool is_const_other,
              typename = std::enable_if_t<(is_const_other == is_const) ||
                                          (!is_const_other && is_const)>>
    list_iterator(list_iterator<is_const_other> const& other)
        : value(other.value) {}

    template <bool is_const_other>
    bool operator==(list_iterator<is_const_other> const& other) const noexcept {
      return value == other.value;
    }

    template <bool is_const_other>
    bool operator!=(list_iterator<is_const_other> const& other) const noexcept {
      return value != other.value;
    }

    pointer operator->() const noexcept {
      return static_cast<pointer>(value);
    }

    reference operator*() const noexcept {
      return static_cast<reference>(*value);
    }

    iterator_class operator++() noexcept {
      value = to_ptr(value->next);
      return *this;
    }

    iterator_class operator++(int) noexcept {
      auto res = *this;
      ++(*this);
      return res;
    }

    iterator_class operator--() noexcept {
      value = to_ptr(value->prev);
      return *this;
    }

    iterator_class operator--(int) noexcept {
      auto res = *this;
      --(*this);
      return res;
    }
  };

  list_element<Tag> church;

  void init() noexcept {
    church.next = &church;
    church.prev = &church;
  }

  void transform_list(list&& other) noexcept {
    church = std::move(other.church);
    other.church.prev = other.church.next = &other.church;
  }

public:
  using iterator = list_iterator<false>;
  using const_iterator = list_iterator<true>;

  list() {
    init();
  }

  list(list const&) = delete;

  list(list&& other) noexcept {
    init();

    if (other.empty()) {
      return;
    }

    transform_list(std::move(other));
  }

  list& operator=(list&& other) noexcept {
    init();

    if (other.empty()) {
      return *this;
    }

    transform_list(std::move(other));

    return *this;
  }

  list& operator=(list const& other) = delete;

  bool empty() const noexcept {
    return (church.next == &church);
  }

  void push_back(tref element) noexcept {
    insert(end(), element);
  }

  void push_front(tref element) noexcept {
    insert(begin(), element);
  }

  void pop_front() noexcept {
    if (!empty()) {
      church.next->unlink();
    }
  }

  void pop_back() noexcept {
    if (!empty()) {
      church.prev->unlink();
    }
  }

  tref front() noexcept {
    return to_ref(*church.next);
  }

  tref back() noexcept {
    return to_ref(*church.prev);
  }

  tcref front() const noexcept {
    return to_ref(*church.next);
  }

  tcref back() const noexcept {
    return to_ref(*church.prev);
  }

  iterator begin() noexcept {
    return iterator(to_ptr(church.next));
  }

  iterator end() noexcept {
    return iterator(&church);
  }

  const_iterator begin() const noexcept {
    return const_iterator(to_ptr(church.next));
  }

  const_iterator end() const noexcept {
    return const_iterator(&church);
  }

  iterator erase(iterator const& it) noexcept {
    obj_ptr point = it.value;
    obj_ptr next = to_ptr(point->next);
    point->unlink();
    return iterator(next);
  }

  iterator insert(const_iterator it, tref element) noexcept {
    obj_ptr target_position = it.value;
    obj_ptr prev = to_ptr(target_position->prev);
    obj_ptr target = to_ptr(&element);
    if (target == target_position || target == prev) {
      return iterator(target_position);
    }
    target->unlink();
    target->next = target_position;
    target->prev = prev;
    prev->next = target;
    target_position->prev = target;
    return iterator(target);
  }

  void splice(const_iterator it, list& /*from*/, const_iterator l,
              const_iterator r) noexcept {
    if (l == r) {
      return;
    }
    obj_ptr range_r = l.value;
    obj_ptr range_l = to_ptr(r.value->prev);
    obj_ptr range_next = r.value;
    obj_ptr range_prev = to_ptr(l.value->prev);
    obj_ptr target_position = it.value;
    obj_ptr prev = to_ptr(target_position->prev);
    range_r->prev = range_l->next = nullptr;
    range_next->prev = range_prev;
    range_prev->next = range_next;
    range_l->next = target_position;
    range_r->prev = prev;
    prev->next = range_r;
    target_position->prev = range_l;
  }
};
} // namespace intrusive
