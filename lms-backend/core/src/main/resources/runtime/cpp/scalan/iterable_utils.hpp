#ifndef __SCALAN_ITERABLE_UTILS_HH__
#define __SCALAN_ITERABLE_UTILS_HH__

#include <functional>
#include <boost/range.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/algorithm.hpp>

namespace scalan 
{

//template<class Pair2Value>
//struct UnPair
//{
//    typedef typename Pair2Value::result_type result_type;
//    typedef typename Pair2Value::argument_type::first_type first_type;
//    typedef typename Pair2Value::argument_type::second_type second_type;
//    
//    static
//    std::function<result_type(first_type,second_type)>
//    doIt( Pair2Value &aFun)
//    {
//        return [&aFun](first_type a, second_type b) -> result_type { return aFun(std::make_pair(a,b)); };
//    }
//};
    
}

#endif //__SCALAN_ITERABLE_UTILS_HH__