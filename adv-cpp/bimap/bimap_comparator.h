#pragma once

#include "bimap_element.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace bimap_details {
template <typename Key, bool Tree, typename Comparator>
struct comparator : Comparator {
  using key_t = Key;
  using base_t = element_base;
  using data_t = element_value<Tree, key_t>;

  void swap(comparator& other) {
    std::swap(static_cast<Comparator&>(*this), static_cast<Comparator&>(other));
  }

  key_t const& get_storage(base_t* node) const {
    return static_cast<data_t*>(node)->get();
  }

  comparator(comparator const& cmp)
      : Comparator(static_cast<Comparator const&>(cmp)) {}
  comparator(comparator&& cmp)
      : Comparator(std::move(static_cast<Comparator&&>(cmp))) {}
  comparator(Comparator const& cmp) : Comparator(cmp) {}
  comparator(Comparator&& cmp) : Comparator(std::move(cmp)) {}

  comparator& operator=(comparator&& other) {
    if (this == &other) {
      return *this;
    }
    comparator(std::move(other)).swap(*this);
    return *this;
  }

  bool operator()(base_t* left, base_t* right) const {
    return this->operator()(get_storage(left), get_storage(right));
  }

  bool operator()(base_t* left, key_t const& right) const {
    return this->operator()(get_storage(left), right);
  }

  bool operator()(key_t const& left, base_t* right) const {
    return this->operator()(left, get_storage(right));
  }

  bool operator()(key_t const& left, key_t const& right) const {
    return Comparator::operator()(left, right);
  }
};
} // namespace bimap_details
