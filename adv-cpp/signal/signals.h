#pragma once
#include <functional>

// Чтобы не было коллизий с UNIX-сигналами реализация вынесена в неймспейс, по
// той же причине изменено и название файла
namespace signals {

template <typename T>
struct signal;

namespace intrusive {

struct connection_tag;

template <typename T, typename Tag = connection_tag>
struct list;

struct list_element_base {
  list_element_base* next = nullptr;
  list_element_base* prev = nullptr;

  list_element_base() = default;
  list_element_base(list_element_base&& other) : next(other.next), prev(other.prev) {
    if (other.next) {
      other.next->prev = this;
    }
    if (other.prev) {
      other.prev->next = this;
    }
    other.next = other.prev = nullptr;
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

  bool is_linked() const noexcept {
    return next && prev;
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
  using reference = T&;

  template <typename Type>
  static list_element<Tag>* to_ptr(Type* ptr) {
    return static_cast<list_element<Tag>*>(ptr);
  }

  template <bool is_const>
  struct list_iterator {
  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = std::conditional_t<is_const, T const, T>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

  private:
    friend struct list;

    list_element<Tag>* value = nullptr;

    explicit list_iterator(list_element<Tag>* value) : value(value) {}
    explicit list_iterator(list_element<Tag> const* value_const) : value(const_cast<list_element<Tag>*>(value_const)) {}

  public:
    template <bool is_const_other,
              typename = std::enable_if_t<(is_const_other == is_const) || (!is_const_other && is_const)>>
    list_iterator(list_iterator<is_const_other> const& other) : value(other.value) {}

    template <bool is_const_other>
    bool operator==(list_iterator<is_const_other> const& other) const noexcept {
      return value == other.value;
    }

    template <bool is_const_other>
    bool operator!=(list_iterator<is_const_other> const& other) const noexcept {
      return value != other.value;
    }

    reference operator*() const noexcept {
      return static_cast<reference>(*value);
    }

    list_iterator operator++() noexcept {
      value = to_ptr(value->next);
      return *this;
    }
    list_iterator operator++(int) noexcept {
      auto res = *this;
      ++(*this);
      return res;
    }

    list_iterator operator--() noexcept {
      value = to_ptr(value->prev);
      return *this;
    }
    list_iterator operator--(int) noexcept {
      auto res = *this;
      --(*this);
      return res;
    }
  };

  list_element<Tag> church;

public:
  using iterator = list_iterator<false>;
  using const_iterator = list_iterator<true>;

  list() {
    church.next = &church;
    church.prev = &church;
  }

  void insert(reference element) noexcept {
    insert(end(), element);
  }

  void clean_front() noexcept {
    church.next->unlink();
  }

  bool empty() const noexcept {
    return (church.next == &church);
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

  iterator get_position(reference element) noexcept {
    return iterator(to_ptr(&element));
  }
  const_iterator get_position(reference element) const noexcept {
    return const_iterator(to_ptr(&element));
  }

  iterator insert(const_iterator it, reference element) noexcept {
    list_element<Tag>* target_position = it.value;
    list_element<Tag>* prev = to_ptr(target_position->prev);
    list_element<Tag>* target = to_ptr(&element);
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
};

} // namespace intrusive

template <typename... Args>
struct signal<void(Args...)> {
private:
  using slot_t = std::function<void(Args...)>;

public:
  struct connection : intrusive::list_element<intrusive::connection_tag> {
  private:
    signal* sig = nullptr;
    slot_t func;

    template <typename T1>
    friend struct signal;

    connection(signal* sig_, slot_t func_) : sig(sig_), func(std::move(func_)) {
      sig->connections.insert(*this);
    }

    void edit_iterator_holder(connection& other) {
      if (!sig || !other.is_linked()) {
        return;
      }
      sig->connections.insert(sig->connections.get_position(other), *this);
      other.disconnect();
    }

  public:
    connection() = default;
    connection(connection&& other) : sig(other.sig), func(std::move(other.func)) {
      edit_iterator_holder(other);
    }

    connection& operator=(connection&& other) {
      if (this == &other) {
        return *this;
      }
      disconnect();
      func = std::move(other.func);
      sig = other.sig;
      edit_iterator_holder(other);
      return *this;
    }

    void disconnect() {
      if (!sig || !this->is_linked()) {
        return;
      }
      for (auto it = sig->tail; it; it = it->prev) {
        if (&(*(it->current)) == this) {
          it->current++;
        }
      }
      this->unlink();
      sig = nullptr;
    }

    void operator()(Args... args) const {
      if (sig) {
        func(std::forward<Args>(args)...);
      }
    }

    ~connection() {
      disconnect();
    }
  };

private:
  using connections_list = intrusive::list<connection>;

  struct iterator_holder {
    explicit iterator_holder(signal* sig_) : sig(sig_), current(sig->connections.begin()), prev(sig->tail) {
      sig->tail = this;
    }

    ~iterator_holder() {
      if (sig && sig->tail) {
        sig->tail = sig->tail->prev;
      }
      sig = nullptr;
    }

    signal* sig = nullptr;
    typename connections_list::const_iterator current;
    iterator_holder* prev = nullptr;
  };

  connections_list connections;
  mutable iterator_holder* tail = nullptr;

public:
  signal() = default;
  signal(signal const&) = delete;
  signal& operator=(signal const&) = delete;

  ~signal() {
    for (auto it = tail; it; it = it->prev) {
      it->sig = nullptr;
    }
    while (!connections.empty()) {
      auto it = connections.begin();
      (*it).func = slot_t();
      connections.clean_front();
    }
  }

  connection connect(std::function<void(Args...)> slot) noexcept {
    return connection(this, std::move(slot));
  }

  void operator()(Args... args) const {
    iterator_holder holder(const_cast<signal*>(this));
    while (holder.current != connections.end()) {
      auto copy(holder.current);
      holder.current++;
      (*copy)(std::forward<Args>(args)...);
      if (!holder.sig) {
        return;
      }
    }
  }
};
} // namespace signals
