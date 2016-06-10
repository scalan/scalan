#ifndef __SCALAN_ALGORITHM_HPP__
#define __SCALAN_ALGORITHM_HPP__

#include <algorithm>

namespace scalan {
    /***
     * Returns:
     * index of the search key, if it is contained in the array;
     * otherwise, (-(insertion point) - 1).
     * The insertion point is defined as the point at which the key would be inserted
     * into the array: the index of the first element greater than the key,
     * or a.length if all elements in the array are less than the specified key.
     * Note that this guarantees that the return value will be >= 0 if and only if
     * the key is found.
    */
    template<class RandomIt, class T>
    int32_t binary_search(RandomIt begin, RandomIt end, const T& val)
    {
        RandomIt i = std::lower_bound(begin, end, val);

        typename
        std::iterator_traits<RandomIt>::difference_type
        idx = std::distance(begin, i);

        if (i != end && !(val < *i))
            return idx;
        else
            return -idx - 1;
    }

}
#endif //__SCALAN_ALGORITHM_HPP__
