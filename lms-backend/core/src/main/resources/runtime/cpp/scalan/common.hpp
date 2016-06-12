#ifndef __SCALAN_COMMON_HPP__
#define __SCALAN_COMMON_HPP__

#include <boost/blank.hpp>

namespace scalan
{

const boost::blank unit_value = boost::blank();

template<class To, class What>
struct Assign {
    static void doIt(To& to, const What& what) {
        to = what;
    }
};

} //namespace scalan

#endif // __SCALAN_COMMON_HPP__
