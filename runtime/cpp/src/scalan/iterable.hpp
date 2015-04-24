#ifndef __SCALAN_ITERABLE_HPP__
#define __SCALAN_ITERABLE_HPP__

#include <scalan/iterable_base.hpp>
#include <scalan/iterable_unit.hpp>
#include <scalan/iterable_create.hpp>
#include <boost/range.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/container/vector.hpp>

namespace scalan {

struct IterableUnitGenJavaIterator
{
    bool hasNext() const {return true;}
    const boost::blank& next() const {return unit_value;}
};


template<class A, class B, class S>
class CreateGenEnumeratorAdaptor
{
    public:
    template<class SrcEnumerator>
    using ResEnumerator = CreateGenEnumerator<SrcEnumerator, B, S>;

    typedef S state;
    typedef function<pair<S,variant<boost::blank, B>>(pair<S,A>)> next_func;
    typedef function<bool(pair<S,A>)> is_complete_func;

    CreateGenEnumeratorAdaptor(next_func aNextFunc, is_complete_func anIsComplete)
    : isComplete(anIsComplete)
    , nextFunc(aNextFunc)
    {}

    template<class SrcEnumerator>
    ResEnumerator<SrcEnumerator> getEnumerator(state aStartState, SrcEnumerator aSrc)
    {
        return CreateGenEnumerator<SrcEnumerator,B,S>(aSrc, aStartState, nextFunc, isComplete);
    }

    private:
    next_func nextFunc;
    is_complete_func isComplete;
};


template<class Range>
class Range2JavaIterator
{
    private:
    typename boost::range_const_iterator<Range>::type srcCur;
    typename boost::range_const_iterator<Range>::type srcEnd;


    public:
    typedef typename boost::range_const_iterator<Range>::type::reference const_reference;

    Range2JavaIterator( const Range& aR ) : srcCur(aR.begin()), srcEnd(aR.end())
    {}

    explicit Range2JavaIterator()
    {}

    const_reference next()
    {
        return *(srcCur++);
    }

    bool hasNext()
    {
        return srcCur != srcEnd;
    }

};

template<class Range>
class Range2Enumerator: public Enumerator<typename boost::range_value<Range>::type>
{
    public:
    typedef typename boost::range_const_iterator<Range>::type iterator;
    typedef typename boost::range_value<Range>::type value_type;
    typedef typename boost::range_const_iterator<Range>::type::reference const_reference;

    Range2Enumerator( const Range& aSrc ) : curIt(aSrc.begin()), endIt(aSrc.end())
    {}

    Range2Enumerator()
    {}

    virtual const_reference current() const {return *curIt;}
    virtual bool moveNext()
    {
        if(start)
        {
            start = false;
            return curIt != endIt;
        }

        if( curIt != endIt )
            ++curIt;

        return curIt != endIt;
    }

    private:
    bool start = true;
    iterator curIt;
    iterator endIt;
};

template<class Range>
class Range2Enumerable: public Enumerable<typename boost::range_value<Range>::type>
{
    public:
    Range2Enumerable(const Range &aSrc )
    {
        pEnumerator = std::make_shared<Range2Enumerator<Range>>(aSrc);
    }

    virtual std::shared_ptr<Enumerator<typename Range::value_type> > enumerator() const
    {
        return pEnumerator;
    }

    private:
    std::shared_ptr<Range2Enumerator<Range>> pEnumerator;
};

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

template<class SourceRange, class B, class S>
boost::iterator_range<create_gen_iterator<Range2Enumerator<SourceRange>,B,S> >
make_gen_range(const SourceRange& aSrc, S aStartState
               , function<pair<S,variant<boost::blank,B>>(pair<S,typename boost::range_value<SourceRange>::type>)> aNextFunc
               , function<bool(pair<S,typename boost::range_value<SourceRange>::type>)> anIsComplete)
{
    auto r2e = std::make_shared<Range2Enumerator<SourceRange>>(Range2Enumerator<SourceRange>(aSrc));
    auto _begin = create_gen_iterator<Range2Enumerator<SourceRange>,B,S>(aStartState, r2e, aNextFunc, anIsComplete);
    return boost::iterator_range<create_gen_iterator<Range2Enumerator<SourceRange>,B,S>>(_begin, _begin.make_end_iterator());
}

} //namespace scalan

#endif // __SCALAN_ITERABLE_HPP__
