#ifndef __SCALAN_ITERABLE_UNIT_HPP__
#define __SCALAN_ITERABLE_UNIT_HPP__

#include <scalan/iterable_base.hpp>

namespace scalan
{

class EnumeratorUnit
{
    public:
    typedef boost::blank value_type;

    const value_type& current() const
    {
        return unit_value;
    }

    bool moveNext()
    {
        return true;
    }
};

const std::shared_ptr<EnumeratorUnit> unitEnumerator = std::make_shared<EnumeratorUnit>();

class EnumerableUnit
{
    public:
    typedef EnumeratorUnit enumerator_t;
    typedef typename enumerator_t::value_type value_type;
        
    std::shared_ptr<EnumeratorUnit> enumerator() const
    {
        return unitEnumerator;
    }
};

} //namespace scalan

#endif // __SCALAN_ITERABLE_UNIT_HPP__
