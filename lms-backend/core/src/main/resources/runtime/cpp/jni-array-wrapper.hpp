#ifndef __JNI_ARRAY_WRAPPER_HPP__
#define __JNI_ARRAY_WRAPPER_HPP__

#include <cstdlib>
#include <jni.h>
#include <iostream>
#include <scalan/common.hpp>

template<class JNIType>
struct jni_type_traits
{
    static constexpr bool isPrimitive = false;
    static const char* className() { return "Ljava/lang/Object;"; }
};

template<>
struct jni_type_traits<jdouble>
{
    static constexpr bool isPrimitive = true;
    static const char* className() { return "D"; }
};

template<>
struct jni_type_traits<jint>
{
    static constexpr bool isPrimitive = true;
    static const char* className() { return "I"; }
};

template<>
struct jni_type_traits<jbyte>
{
    static constexpr bool isPrimitive = true;
    static const char* className() { return "B"; }
};

template<>
struct jni_type_traits<jdoubleArray>
{
    static constexpr bool isPrimitive = false;
    static const char* className() { return "[D"; }
};

template<>
struct jni_type_traits<jbyteArray>
{
    static constexpr bool isPrimitive = false;
    static const char* className() { return "[B"; }
};

template<class T>
struct jni_type
{
    typedef jobject type;
    typedef jobjectArray array_type;
};

template<>
struct jni_type<int8_t>
{
    typedef jbyte type;
    typedef jbyteArray array_type;
};

template<>
struct jni_type<int32_t>
{
    typedef jint type;
    typedef jintArray array_type;
};

template<>
struct jni_type<double>
{
    typedef jdouble type;
    typedef jdoubleArray array_type;
};


template<class ElemType, bool isPrimitive>
struct __JNIArray
{
    typedef typename jni_type<ElemType>::type elem_type;
    typedef typename jni_type<ElemType>::array_type type;

    static ElemType get( const JNIEnv *env, const type anArray, int32_t index )
    {
        return const_cast<JNIEnv*>(env)->GetObjectArrayElement(anArray, index);
    }
};

// template specialization for Java primitive types
// ElemType is C++ type
template<class ElemType>
struct __JNIArray<ElemType, true>
{
    typedef typename jni_type<ElemType>::type elem_type;
    typedef typename jni_type<ElemType>::array_type type;

    static ElemType *getElements(const JNIEnv *env, const type anArray, jboolean *isCopy)
    {
        return reinterpret_cast<ElemType*>( const_cast<JNIEnv*>(env)->GetPrimitiveArrayCritical( anArray, isCopy ) );
    }

    static void release(const JNIEnv *env, const type anArray, const elem_type* anElems, jint aMode )
    {
        const_cast<JNIEnv*>(env)->ReleasePrimitiveArrayCritical(anArray, const_cast<elem_type*>(anElems), aMode );
    }
};

template<class ElemType>
using JNIArray = __JNIArray<ElemType, jni_type_traits<typename jni_type<ElemType>::type>::isPrimitive>;

template<class T>
class jni_array
{
public:
    typedef typename jni_type<T>::type jni_t;
    typedef typename jni_type<T>::array_type jni_array_t;
    typedef const T* const_iterator;

    jni_array() : env(nullptr), jniArray(nullptr), raw_array(nullptr), sz(0)
    {}


    explicit
    jni_array( const JNIEnv *anEnv, const jni_array_t& aJNIArray ) : env(anEnv) , jniArray(aJNIArray) , sz(const_cast<JNIEnv*>(this->env)->GetArrayLength( this->jniArray ))
    {
        raw_array = JNIArray<T>::getElements( this->env, this->jniArray, nullptr );
        //	std::cout << "jni_array( const JNIEnv *anEnv, const jni_array_t& aJNIArray ): " << raw_array << " " << sizeof(T) << std::endl;
    }

    jni_array( const jni_array& anArray ) = delete;

    jni_array( jni_array&& anArray ) : env(anArray.env), jniArray(anArray.jniArray), sz(anArray.sz)
    {
        this->raw_array = anArray.raw_array;
        anArray.reset();
        //	std::cout << "jni_array( jni_array&& anArray ): " << raw_array << std::endl;
    }

    ~jni_array()
    {
        //	std::cout << "~jni_array(): " << raw_array << std::endl;
        if( this->env != nullptr && this->raw_array != nullptr )
        {
            JNIArray<T>::release( this->env, this->jniArray, this->raw_array, 0 );
        }
    }

    const_iterator begin() const { return raw_array; }

    const_iterator end() const { return raw_array + this->sz; }

    int32_t size() const { return this->sz; }

    const T& operator[]( size_t idx ) const
    {
        return this->raw_array[idx];
    }

    T& operator[]( size_t idx )
    {
        return this->raw_array[idx];
    }

    const jni_array<T>& operator=( jni_array&& anArray )
    {
        //	std::cout << "jni_array<T>& operator=( jni_array&& anArray )" << std::endl;
        this->reset( anArray.env, anArray.jniArray, anArray.sz, anArray.raw_array );
        anArray.reset();
        return *this;
    }



private:
    const JNIEnv* env;
    const jni_array_t jniArray;
	T* raw_array;

    const int32_t sz;

    void reset( const JNIEnv* e0 = nullptr, const jni_array_t a0 = nullptr, int32_t sz0 = 0, const T* raw_array0 = nullptr )
    {
        const_cast<JNIEnv*&>(env) = const_cast<JNIEnv*>(e0);
        const_cast<jni_array_t&>(jniArray) = const_cast<jni_array_t>(a0);
        const_cast<int32_t&>(sz) = sz0;
        const_cast<T*&>(raw_array) = const_cast<T*>(raw_array0);
    }
};

namespace scalan {

template<>
struct Assign<std::shared_ptr<boost::container::vector<double>>,std::shared_ptr<jni_array<double>>> {
    static void doIt(std::shared_ptr<boost::container::vector<double>>& to, std::shared_ptr<jni_array<double>> what)
    {
        to.reset(new boost::container::vector<double>(what->size(), boost::container::default_init));
        for(size_t i = 0; i < to->size(); ++i) {
            (*to)[i] = (*what)[i];
        }
    }
};

} // namespace scalan

#endif // __JNI_ARRAY_WRAPPER_HPP___
