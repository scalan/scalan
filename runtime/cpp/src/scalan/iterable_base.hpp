#ifndef __SCALAN_ITERABLE_BASE_HPP__
#define __SCALAN_ITERABLE_BASE_HPP__

#include <functional>
#include <memory>
#include <utility>
#include <boost/blank.hpp>
#include <boost/variant/variant.hpp>

namespace scalan
{

using std::function;
using std::pair;
using boost::variant;

const boost::blank unit_value;

template<class T>
class Enumerator {
    public:
    typedef T value_type;

    virtual const T& current() const = 0;
    virtual bool moveNext() = 0;
};

template<class T>
class Enumerable
{
    public:
    typedef T value_type;
    typedef Enumerator<T> enumerator_t;
    virtual std::shared_ptr<enumerator_t> enumerator() const = 0;
};

} //namespace scalan

#endif // __SCALAN_ITERABLE_BASE_HPP__
