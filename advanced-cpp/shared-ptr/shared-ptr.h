#pragma once

#include <cstddef>
#include <memory>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template <typename T>
class shared_ptr;

template <typename T>
class weak_ptr;

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&... args);

struct control_block {
private:
  std::size_t cnt_strong = 0;
  std::size_t cnt_weak = 0;

  void update_strong() noexcept {
    if (empty_strong()) {
      unlink();
    }
    update_weak();
  }

  void update_weak() noexcept {
    if (empty_summary()) {
      delete this;
    }
  }

public:
  virtual ~control_block() = default;
  virtual void unlink() = 0;

  std::size_t use_count() const noexcept {
    return cnt_strong;
  }

  void add_strong() noexcept {
    cnt_strong++;
  }

  void sub_strong() noexcept {
    cnt_strong--;
    update_strong();
  }

  void add_weak() noexcept {
    cnt_weak++;
  }

  void sub_weak() noexcept {
    cnt_weak--;
    update_weak();
  }

  bool empty_strong() const noexcept {
    return cnt_strong == 0;
  }

  bool empty_summary() const noexcept {
    return cnt_strong + cnt_weak == 0;
  }
};

template <typename T, typename D = std::default_delete<T>>
struct ptr_block : control_block, D {
private:
  T* p = nullptr;

public:
  ptr_block(T* ptr, D deleter = std::default_delete<T>())
      : p(ptr), D(std::move(deleter)) {}

  void unlink() override {
    D::operator()(p);
    p = nullptr;
  }

  T* get_ptr() {
    return p;
  }
};

template <typename T>
struct obj_block : control_block {
private:
  std::aligned_storage_t<sizeof(T), alignof(T)> o;

public:
  template <typename... Args>
  obj_block(Args&&... args) {
    new (&o) T(std::forward<T>(args)...);
  }

  void unlink() override {
    get_ptr()->~T();
  }

  T* get_ptr() {
    return reinterpret_cast<T*>(&o);
  }
};

template <typename T>
class shared_ptr {
private:
  template <typename R, typename... Args>
  friend shared_ptr<R> make_shared(Args&&... args);

  friend class weak_ptr<T>;

  template <typename D>
  friend class shared_ptr;

  control_block* cb = nullptr;
  T* ptr = nullptr;

  shared_ptr(control_block* other, T* ptr) noexcept : cb(other), ptr(ptr) {
    if (cb)
      cb->add_strong();
  }

public:
  void swap(shared_ptr& from) noexcept {
    std::swap(cb, from.cb);
    std::swap(ptr, from.ptr);
  }

  shared_ptr() noexcept = default;

  shared_ptr(std::nullptr_t) noexcept {}

  shared_ptr(shared_ptr const& other) noexcept : ptr(other.ptr), cb(other.cb) {
    if (cb)
      cb->add_strong();
  }

  shared_ptr(shared_ptr&& other) noexcept : ptr(other.ptr), cb(other.cb) {
    other.ptr = nullptr;
    other.cb = nullptr;
  }

  template <typename D>
  shared_ptr(shared_ptr<D> const& other) noexcept
      : cb(other.cb), ptr(other.ptr) {
    if (cb)
      cb->add_strong();
  }

  template <typename D>
  shared_ptr(shared_ptr<D> const& other, T* element) noexcept
      : cb(other.cb), ptr(element) {
    if (cb)
      cb->add_strong();
  }

  template <typename TT, typename D = std::default_delete<TT>>
  shared_ptr(TT* ptr_, D deleter = std::default_delete<TT>()) {
    try {
      cb = new ptr_block<TT, D>(ptr_, std::move(deleter));
      ptr = ptr_;
      cb->add_strong();
    } catch (...) {
      deleter(ptr_);
      throw;
    }
  }

  shared_ptr& operator=(shared_ptr const& other) noexcept {
    if (this == &other)
      return *this;
    shared_ptr(other).swap(*this);
    return *this;
  }

  shared_ptr& operator=(shared_ptr&& other) noexcept {
    if (this == &other)
      return *this;
    shared_ptr(std::move(other)).swap(*this);
    return *this;
  }

  ~shared_ptr() noexcept {
    if (cb) {
      cb->sub_strong();
    }
    cb = nullptr;
    ptr = nullptr;
  }

  bool operator==(std::nullptr_t) const noexcept {
    return get() == nullptr;
  }

  bool operator!=(std::nullptr_t) const noexcept {
    return get() != nullptr;
  }

  friend bool operator==(std::nullptr_t, shared_ptr const& other) noexcept {
    return other.get() == nullptr;
  }

  friend bool operator!=(std::nullptr_t, shared_ptr const& other) noexcept {
    return other.get() != nullptr;
  }

  T* get() const noexcept {
    return ptr;
  }

  operator bool() const noexcept {
    return ptr;
  }

  T& operator*() const noexcept {
    return *get();
  }

  T* operator->() const noexcept {
    return get();
  }

  std::size_t use_count() const noexcept {
    return cb ? cb->use_count() : 0;
  }

  void reset() noexcept {
    shared_ptr().swap(*this);
  }

  template <typename TT, typename DT = std::default_delete<TT>>
  void reset(TT* new_ptr, DT deleter = std::default_delete<TT>()) {
    shared_ptr(new_ptr, std::move(deleter)).swap(*this);
  }
};

template <typename T>
class weak_ptr {
private:
  control_block* cb = nullptr;
  T* ptr = nullptr;

public:
  void swap(weak_ptr& from) noexcept {
    std::swap(cb, from.cb);
    std::swap(ptr, from.ptr);
  }

  weak_ptr() noexcept = default;

  weak_ptr(shared_ptr<T> const& other) noexcept : cb(other.cb), ptr(other.ptr) {
    if (cb)
      cb->add_weak();
  }

  weak_ptr(weak_ptr const& other) noexcept : cb(other.cb), ptr(other.ptr) {
    if (cb)
      cb->add_weak();
  }

  weak_ptr(weak_ptr&& other) noexcept : cb(other.cb), ptr(other.ptr) {
    other.ptr = nullptr;
    other.cb = nullptr;
  }

  weak_ptr& operator=(shared_ptr<T> const& other) noexcept {
    if (cb == other.cb)
      return *this;
    weak_ptr(other).swap(*this);
    return *this;
  }

  weak_ptr& operator=(weak_ptr const& other) noexcept {
    if (this == &other)
      return *this;
    weak_ptr(other).swap(*this);
    return *this;
  }

  weak_ptr& operator=(weak_ptr&& other) noexcept {
    if (this == &other)
      return *this;
    weak_ptr(std::move(other)).swap(*this);
    return *this;
  }

  ~weak_ptr() {
    if (cb) {
      cb->sub_weak();
    }
    cb = nullptr;
    ptr = nullptr;
  }

  shared_ptr<T> lock() const noexcept {
    return (cb && !cb->empty_strong() ? shared_ptr<T>(cb, ptr)
                                      : shared_ptr<T>());
  }
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&... args) {
  auto* obj = new obj_block<T>(std::forward<Args...>(args)...);
  return shared_ptr(static_cast<control_block*>(obj), obj->get_ptr());
}
