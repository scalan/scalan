#ifndef __SCALAN_ALGORITHM_HPP__
#define __SCALAN_ALGORITHM_HPP__

#include <algorithm>

namespace scalan {

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
