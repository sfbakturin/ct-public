#pragma once

#include <type_traits>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace details::concepts {
template <typename... Types>
concept TriviallyCopyConstructible = std::conjunction_v<std::is_trivially_copy_constructible<Types>...>;

template <typename... Types>
concept TriviallyMoveConstructible = std::conjunction_v<std::is_trivially_move_constructible<Types>...>;

template <typename... Types>
concept CopyConstructible = std::conjunction_v<std::is_copy_constructible<Types>...>;

template <typename... Types>
concept MoveConstructible = std::conjunction_v<std::is_move_constructible<Types>...>;

template <typename... Types>
concept TriviallyCopyAssignable =
    std::conjunction_v<std::is_trivially_copy_assignable<Types>...> && TriviallyCopyConstructible<Types...>;

template <typename... Types>
concept TriviallyMoveAssignable =
    std::conjunction_v<std::is_trivially_move_assignable<Types>...> && TriviallyMoveConstructible<Types...>;

template <typename... Types>
concept CopyAssignable = std::conjunction_v<std::is_copy_assignable<Types>...> && CopyConstructible<Types...>;

template <typename... Types>
concept MoveAssignable = std::conjunction_v<std::is_move_assignable<Types>...> && MoveConstructible<Types...>;
} // namespace details::concepts
