#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

inline constexpr std::size_t variant_npos = -1;

struct bad_variant_access : std::exception {
  char const* what() const noexcept override {
    return "bad_variant_access";
  }
};
