#ifndef __SCALAN_ITERABLE_UNIT_HPP__
#define __SCALAN_ITERABLE_UNIT_HPP__

#include <scalan/iterable_base.hpp>

namespace scalan
{

class EnumeratorUnit: public Enumerator<boost::blank>
{
    public:
    typedef boost::blank value_type;

    virtual const value_type& current() const
    {
        return unit_value;
    }

    virtual bool moveNext()
    {
        return true;
    }
};

const std::shared_ptr<Enumerator<boost::blank>> unitEnumerator = std::make_shared<EnumeratorUnit>();

class EnumerableUnit: public Enumerable<boost::blank>
{
    public:
    virtual std::shared_ptr<Enumerator<boost::blank>> enumerator() const
    {
        return unitEnumerator;
    }
};

} //namespace scalan

#endif // __SCALAN_ITERABLE_UNIT_HPP__
