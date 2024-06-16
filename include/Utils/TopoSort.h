#pragma once

#include <vector>
#include <ranges>

/// topological sort, Kahn's algorithm
template <std::random_access_iterator I, class S, class F>
void topological_sort(I first, S last, F edge) {
    const std::size_t n = std::ranges::distance(first, last);
    std::vector<std::size_t> in_degree(n);

    for (std::size_t i = 0; i < n; ++i) {
        for (std::size_t j = 0; j < n; ++j) {
            in_degree[i] += static_cast<bool>(edge(first[j], first[i]));
        }
    }

    // [s_first, s_last) are the sources of the sub-graph [s_first, last)
    auto s_first = first;
    auto s_last = s_first;

    for (std::size_t i = 0; i < n; ++i) {
        if (in_degree[i] == 0) {
            std::swap(first[i], *s_last);
            std::swap(in_degree[i], in_degree[s_last - first]);
            ++s_last;
        }
    }

    for (; s_first != s_last; ++s_first) {
        for (auto t_it = s_last; t_it != last; ++t_it) {
            if (edge(*s_first, *t_it) && --in_degree[t_it - first] == 0) {
                std::swap(*t_it, *s_last);
                std::swap(in_degree[t_it - first], in_degree[s_last - first]);
                ++s_last;
            }
        }
    }
}
