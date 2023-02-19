#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct nullopt_t {};

struct in_place_t {};

inline constexpr in_place_t in_place{};
inline constexpr nullopt_t nullopt{};
