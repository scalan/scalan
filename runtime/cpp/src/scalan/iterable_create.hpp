#ifndef __SCALAN_ITERABLE_CREATE_HPP
#define __SCALAN_ITERABLE_CREATE_HPP

#include <scalan/iterable_base.hpp>
#include <boost/variant/get.hpp>
#include <boost/iterator/iterator_facade.hpp>

namespace scalan
{

template<class SrcEnumerator, class B, class S>
class CreateGenEnumerator: public Enumerator<B>
{
    public:
    typedef typename SrcEnumerator::value_type A;
    typedef S state;
    typedef function<pair<S,variant<boost::blank, B>>(pair<S,A>)> next_func;
    typedef function<bool(pair<S,A>)> is_complete_func;
    typedef typename std::add_lvalue_reference<const B>::type const_reference;

    CreateGenEnumerator(state aStartState, const std::shared_ptr<SrcEnumerator>& aSrc, const next_func& aNextFunc, const is_complete_func& anIsComplete)
        : src(aSrc)
        , curState(aStartState)
        , nextFunc(aNextFunc)
        , isComplete(anIsComplete)
    {}

    CreateGenEnumerator() : complete(true)
    {}

    const_reference current() const { return boost::get<B>( curOut ); }

    bool moveNext()
    {
        if(complete)
            return false;

        if( !src->moveNext() )
        {
            complete = true;
            curOut = unit_value;
            return false;
        }

        auto srcNext = src->current();
        std::cout << "before isComplete()" << std::endl;
        if( isComplete(std::make_pair(curState, srcNext)) )
        {
            std::cout << "after isComplete()" << std::endl;
            complete = true;
            curOut = unit_value;
            return false;
        }
        std::cout << "after isComplete()" << std::endl;

        while( true )
        {
            std::cout << "before nextFunc()" << std::endl;
            pair<S,variant<boost::blank,B>>
            p = nextFunc(std::make_pair(curState, srcNext));
            std::cout << "after nextFunc()" << std::endl;
            curState = std::move(p.first);
            curOut = std::move(p.second);
            if( boost::get<B>( &curOut ) != nullptr )
                return true;

            if(!src->moveNext())
            {
                curOut = unit_value;
                return false;
            }

            srcNext = src->current();
        }
    }

    bool operator==(const CreateGenEnumerator &other)
    {
        return complete == other.complete && curOut == other.curOut;
    }

    private:
    bool complete = false;
    variant<boost::blank,B> curOut = unit_value;
    std::shared_ptr<SrcEnumerator> src;
    state curState;
    next_func nextFunc;
    is_complete_func isComplete;
};

template<class A, class B, class S>
class EnumerableCreate: public Enumerable<B>
{
    public:
    typedef S state;
    typedef B value_type;
    typedef function<pair<S,variant<boost::blank, B>>(pair<S,A>)> next_func;
    typedef function<bool(pair<S,A>)> is_complete_func;
    typedef Enumerable<A> src_enumerable;
    typedef CreateGenEnumerator<typename src_enumerable::enumerator_t, value_type, state> enumerator_t;
    

    EnumerableCreate( state aStartState, const std::shared_ptr<src_enumerable> &aSrc, const next_func& aNextFunc, const is_complete_func& anIsComplete)
    : s(aStartState), src(aSrc), next(aNextFunc), is_complete(anIsComplete)
    {}

    virtual std::shared_ptr<Enumerator<value_type>> enumerator() const
    {
        return std::make_shared<enumerator_t>(s, src->enumerator(), next, is_complete);
    }

    private:
    state s;
    std::shared_ptr<src_enumerable> src;
    next_func next;
    is_complete_func is_complete;
    
        
};

template<class SrcEnumerator, class B, class S>
class create_gen_iterator : public boost::iterator_facade<create_gen_iterator<SrcEnumerator,B,S>, B*, boost::forward_traversal_tag, typename std::add_lvalue_reference<typename std::add_const<B>::type>::type>
{
    public:
    typedef typename SrcEnumerator::value_type A;
    typedef S state;
    typedef function<pair<S,variant<boost::blank, B>>(pair<S,A>)> next_func;
    typedef function<bool(pair<S,A>)> is_complete_func;

    private:
    typedef CreateGenEnumerator<SrcEnumerator,B,S> result_enumerator;

    std::shared_ptr<result_enumerator> resEnumerator;

    explicit create_gen_iterator() : resEnumerator(std::make_shared<result_enumerator>())
    {}

    public:
    create_gen_iterator(state aStartState, const std::shared_ptr<SrcEnumerator> aSrc, next_func aNextFunc, is_complete_func anIsComplete)
    {
        resEnumerator.reset( new result_enumerator(aStartState, aSrc, aNextFunc, anIsComplete) );
        resEnumerator->moveNext();
    }

    create_gen_iterator make_end_iterator()
    {
        return create_gen_iterator();
    }

    bool equal(const create_gen_iterator& other) const
    {
        return *this->resEnumerator == *other.resEnumerator;
    }

    void increment()
    {
        resEnumerator->moveNext();
    }

    typename create_gen_iterator::reference dereference() const { return resEnumerator->current(); }
};

} //namespace scalan

#endif // __SCALAN_ITERABLE_CREATE_HPP
