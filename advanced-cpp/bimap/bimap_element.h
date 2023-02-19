#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace bimap_details {
struct element_base {
  element_base* left = nullptr;
  element_base* right = nullptr;
  element_base* parent = nullptr;

  void relink_parent(element_base* other) {
    if (parent) {
      if (parent->right == this) {
        parent->right = other;
      } else {
        parent->left = other;
      }
    }
  }

  void relink_child() {
    if (left) {
      left->parent = this;
    }
    if (right) {
      right->parent = this;
    }
  }

  void relink_self(element_base& other) {
    std::swap(left, other.left);
    std::swap(right, other.right);
    std::swap(parent, other.parent);
  }

  void swap(element_base& other) {
    relink_parent(&other);
    other.relink_parent(this);
    relink_self(other);
    relink_child();
    other.relink_child();
  }

  element_base() = default;
  element_base(element_base&& other) {
    swap(other);
  }

  element_base& operator=(element_base&& other) {
    if (this == &other) {
      return *this;
    }
    element_base(std::move(other)).swap(*this);
    return *this;
  }

  element_base* min(element_base* node) const {
    if (node->left) {
      return min(node->left);
    } else {
      return node;
    }
  }

  element_base* max(element_base* node) const {
    if (node->right) {
      return max(node->right);
    } else {
      return node;
    }
  }

  element_base* next(element_base* node) const {
    if (!node->right) {
      if (node->parent->left == node) {
        node = node->parent;
      } else {
        element_base* transport = node->parent;
        while (transport && transport->right == node) {
          node = transport;
          transport = transport->parent;
        }
        if (transport) {
          node = transport;
        }
      }
    } else {
      node = node->right;
      node = min(node);
    }
    return node;
  }

  element_base* prev(element_base* node) const {
    if (!node->left) {
      if (node->parent->right == node) {
        node = node->parent;
      } else {
        element_base* transport = node->parent;
        while (transport && transport->left == node) {
          node = transport;
          transport = transport->parent;
        }
        if (transport) {
          node = transport;
        }
      }
    } else {
      node = node->left;
      node = max(node);
    }
    return node;
  }
};

template <bool Tree, typename Storage>
struct element_value : element_base {
  Storage storage;

  element_value(Storage const& storage) : storage(storage) {}
  element_value(Storage&& storage) : storage(std::move(storage)) {}

  Storage const& get() const {
    return storage;
  }
  Storage& get() {
    return storage;
  }
};

template <typename Key, typename Value>
struct element_data : element_value<true, Key>, element_value<false, Value> {
  template <typename Key_f = Key, typename Value_f = Value>
  element_data(Key_f&& key, Value_f&& value)
      : element_value<true, Key>(std::forward<Key_f>(key)),
        element_value<false, Value>(std::forward<Value_f>(value)) {}
};
} // namespace bimap_details
