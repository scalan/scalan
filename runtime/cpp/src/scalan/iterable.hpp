#ifndef __SCALAN_ITERABLE_HPP__
#define __SCALAN_ITERABLE_HPP__

#include <scalan/iterable_base.hpp>
#include <scalan/iterable_unit.hpp>
#include <scalan/iterable_create.hpp>
#include <scalan/iterable_utils.hpp>
#include <boost/container/vector.hpp>

namespace scalan {

template<class EnumerableT>
std::shared_ptr<boost::container::vector<typename EnumerableT::value_type>>
toArray( const EnumerableT& anIn )
{
    std::shared_ptr<boost::container::vector<typename EnumerableT::value_type> >
    res = std::make_shared<boost::container::vector<typename EnumerableT::value_type> >();

    auto en = anIn.enumerator();
    while(en->moveNext())
    {
        res->push_back( en->current() );
    }

    return res;
}

template<class EnumerableT, class R>
R foldLeft(const EnumerableT& anIn, R z, std::function<R( std::pair<R,typename EnumerableT::value_type> )> f)
{
    auto en = anIn.enumerator();
    while(en->moveNext())
    {
        z = f( std::make_pair(z, en->current()) );
    }

    return z;    
}

template<class EnumerableT>
typename EnumerableT::value_type last(const EnumerableT& anIt )
{
    auto en = anIt.enumerator();
    if( en->moveNext() )
    {
        typename EnumerableT::value_type res = en->current();
        while(en->moveNext())
        {
            res = en->current();
        }

        return res;
    }
    else
        throw std::out_of_range("empty iterator");
}

} //namespace scalan

#endif // __SCALAN_ITERABLE_HPP__
