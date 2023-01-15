/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template <typename>
struct intrusive_ref_counter;

template <typename T>
struct intrusive_ptr {
private:
  using element_type = T;

  template <typename>
  friend struct intrusive_ptr;

  element_type* ptr = nullptr;

public:
  intrusive_ptr() noexcept = default;
  intrusive_ptr(T* p, bool add_ref = true) : ptr(p) {
    if (p && add_ref) {
      intrusive_ptr_add_ref(p);
    }
  }

  intrusive_ptr(intrusive_ptr const& r) : ptr(r.ptr) {
    if (r.get()) {
      intrusive_ptr_add_ref(r.get());
    }
  }
  template <class Y>
  requires(std::is_convertible_v<Y*, T*>)
      intrusive_ptr(intrusive_ptr<Y> const& r)
      : ptr(static_cast<T*>(r.get())) {
    if (r.get()) {
      intrusive_ptr_add_ref(r.get());
    }
  }

  intrusive_ptr(intrusive_ptr&& r) : ptr(r.ptr) {
    r.ptr = nullptr;
  }
  template <class Y>
  requires(std::is_convertible_v<Y*, T*>) intrusive_ptr(intrusive_ptr<Y>&& r)
      : ptr(static_cast<T*>(r.get())) {
    r.ptr = nullptr;
  }

  ~intrusive_ptr() {
    if (get()) {
      intrusive_ptr_release(get());
    }
  }

  intrusive_ptr& operator=(intrusive_ptr const& r) {
    if (this == &r) {
      return *this;
    }
    intrusive_ptr(r).swap(*this);
    return *this;
  }
  template <class Y>
  requires(std::is_convertible_v<Y*, T*>) intrusive_ptr&
  operator=(intrusive_ptr<Y> const& r) {
    intrusive_ptr(r).swap(*this);
    return *this;
  }
  intrusive_ptr& operator=(T* r) {
    intrusive_ptr(r).swap(*this);
    return *this;
  }

  intrusive_ptr& operator=(intrusive_ptr&& r) {
    if (this == &r) {
      return *this;
    }
    instruive_ptr(std::move(r)).swap(*this);
    return *this;
  }
  template <class Y>
  requires(std::is_convertible_v<Y*, T*>) intrusive_ptr&
  operator=(intrusive_ptr<Y>&& r) {
    intrusive_ptr(std::move(r)).swap(*this);
    return *this;
  }

  void reset() {
    intrusive_ptr().swap(*this);
  }
  void reset(T* r) {
    intrusive_ptr(r).swap(*this);
  }
  void reset(T* r, bool add_ref) {
    intrusive_ptr(r, add_ref).swap(*this);
  }

  T& operator*() const noexcept {
    if (get()) {
      return *get();
    }
    assert(false);
  }
  T* operator->() const noexcept {
    return get();
  }
  T* get() const noexcept {
    return ptr;
  }
  T* detach() noexcept {
    T* res = ptr;
    ptr = nullptr;
    return res;
  }

  explicit operator bool() const noexcept {
    return get();
  }

  void swap(intrusive_ptr& b) noexcept {
    using std::swap;
    swap(ptr, b.ptr);
  }
};

template <class T, class U>
bool operator==(intrusive_ptr<T> const& a, intrusive_ptr<U> const& b) noexcept {
  return a.get() == b.get();
}

template <class T, class U>
bool operator!=(intrusive_ptr<T> const& a, intrusive_ptr<U> const& b) noexcept {
  return a.get() != b.get();
}

template <class T, class U>
bool operator==(intrusive_ptr<T> const& a, U* b) noexcept {
  return a.get() == b;
}

template <class T, class U>
bool operator!=(intrusive_ptr<T> const& a, U* b) noexcept {
  return a.get() != b;
}

template <class T, class U>
bool operator==(T* a, intrusive_ptr<U> const& b) noexcept {
  return a == b.get();
}

template <class T, class U>
bool operator!=(T* a, intrusive_ptr<U> const& b) noexcept {
  return a != b.get();
}

template <class T>
bool operator<(intrusive_ptr<T> const& a, intrusive_ptr<T> const& b) noexcept {
  return std::less<T*>()(a.get(), b.get());
}

template <class T>
void swap(intrusive_ptr<T>& a, intrusive_ptr<T>& b) noexcept {
  a.swap(b);
}

template <typename T>
struct intrusive_ref_counter {
private:
  template <typename D1>
  friend void intrusive_ptr_add_ref(intrusive_ref_counter<D1> const*) noexcept;

  template <typename D2>
  friend void intrusive_ptr_release(intrusive_ref_counter<D2> const*) noexcept;

  mutable std::atomic<std::size_t> count = 0;

public:
  intrusive_ref_counter() noexcept = default;
  intrusive_ref_counter(intrusive_ref_counter const& v) noexcept {}

  intrusive_ref_counter& operator=(intrusive_ref_counter const& v) noexcept {
    return *this;
  }

  unsigned int use_count() const noexcept {
    return count.load(std::memory_order_acquire);
  }

protected:
  ~intrusive_ref_counter() = default;
};

template <class Derived>
void intrusive_ptr_add_ref(intrusive_ref_counter<Derived> const* p) noexcept {
  p->count.fetch_add(1, std::memory_order_relaxed);
}

template <class Derived>
void intrusive_ptr_release(intrusive_ref_counter<Derived> const* p) noexcept {
  if (p->count.fetch_sub(1, std::memory_order_acq_rel) == 1) {
    delete p;
  }
}
