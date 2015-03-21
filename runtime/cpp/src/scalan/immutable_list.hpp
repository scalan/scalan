#ifndef __SCALAN_IMMUTABLE_LIST_HPP__
#define __SCALAN_IMMUTABLE_LIST_HPP__

#include <memory>
#include <type_traits>
#include <boost/iterator/iterator_facade.hpp>


namespace scalan
{
    using std::shared_ptr;
    namespace detail
    {
        template<class T>
        struct immutable_list_node
        {
            typedef T value_type;

            value_type val;
            const shared_ptr<immutable_list_node<value_type>> next;

            immutable_list_node() : val(value_type()) {}
            immutable_list_node( const value_type &aV, const shared_ptr<immutable_list_node<value_type>> &aTail ): val(aV), next(aTail) {}
        };

        template<class P, class V>
        class immutable_list_iterator_base: public boost::iterator_facade<immutable_list_iterator_base<P,V>, P, boost::forward_traversal_tag, typename std::add_lvalue_reference<V>::type>
        {
        public:
            immutable_list_iterator_base(const shared_ptr<P>& aCur) : cur(aCur) {}

        private:
            shared_ptr<P> cur;

            friend class boost::iterator_core_access;

            void increment() { cur = cur->next; }

            bool equal(const immutable_list_iterator_base& other) const
            {
                return this->cur == other.cur;
            }

            typename immutable_list_iterator_base::reference dereference() const { return cur->val; }
        }; //class immutable_list_iterator_base
    }

    template<class T>
    class immutable_list
    {
    private:
        typedef detail::immutable_list_node<T> node_t;

    public:
        typedef T value_type;
        typedef T& reference;
        typedef const T& const_reference;
        typedef detail::immutable_list_iterator_base<node_t,T> iterator;
        typedef detail::immutable_list_iterator_base<typename std::add_const<node_t>::type, typename std::add_const<T>::type> const_iterator;

        immutable_list() : len(0) {}

        immutable_list(const_reference aV, const immutable_list<T>& aTail): len(aTail.len + 1), h(std::make_shared<node_t>(aV, aTail.h)) {}

        immutable_list(const immutable_list<T>& aFirst, const immutable_list<T>& aSecond) {
            auto l = loop( ++aFirst.begin(), aFirst.end(), aSecond );
            this->len = l.len + 1;
            this->h = std::make_shared<node_t>(*aFirst.begin(), l.h);
        }

        immutable_list(std::initializer_list<T> args)
        {
            auto l = loop( args.begin()+1, args.end(), immutable_list() );
            this->len = l.len + 1;
            this->h = std::make_shared<node_t>(*args.begin(), l.h);
        }

        immutable_list(size_t aLen, const_reference aV)
        {
            auto l = immutable_list();
            for( size_t i = 1; i < aLen; ++i )
            {
                l = immutable_list(aV, l);
            }

            this->len = l.len + 1;
            this->h = std::make_shared<node_t>(aV, l.h);
        }

        bool empty() const
        {
            return !this->h; // implicit conversion via shared_ptr::operator bool()
        }

        reference front() { return this->h->val; }
        const_reference front() const { return this->h->val; }

        immutable_list<T> push_front(const_reference aV) const
        {
            return immutable_list(aV, *this);
        }

        immutable_list<T> pop_front() const
        {
            return this->empty() ? immutable_list() : immutable_list(this->h->next);
        }

        iterator begin() const
        {
            return iterator(h);
        }

        iterator end() const
        {
            return iterator(immutable_list().h);
        }

        const_iterator cbegin() const
        {
            return const_iterator(h);
        }

        const_iterator cend() const
        {
            return const_iterator(immutable_list().h);
        }

        size_t length() const { return this->len; }

        void set_tail( immutable_list& aTail )
        {
            const_cast<shared_ptr<node_t>&>(this->h->next) = aTail.h;
            this->len += aTail.len;
        }

        void set_len(size_t aLen)
        {
            this->len = aLen;
        }

    private:
        size_t len;
        shared_ptr<node_t> h;

        explicit immutable_list(shared_ptr<node_t> aHead): h(aHead) {}

        template<class Iterator>
        immutable_list loop(Iterator cur, Iterator end, const immutable_list& tail)
        {
            if( cur != end )
            {
                auto val = *cur;
                auto l = loop(++cur, end, tail);
                return l.push_front(val);
            }
            else
                return tail;
        }
    };
} //namespace scalan

#endif //__SCALAN_IMMUTABLE_LIST_HPP__
