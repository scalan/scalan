#include <jni.h>
#include <tuple>
#include <iostream>
#include <jni-array-wrapper.hpp>
#include <stdexcept>
#include <vector>

using namespace std;

template<class T> struct Extractor;
template<class T> struct GetField;

jmethodID getMethodID( JNIEnv *env, jobject& anObj, const char* aMN, const char* aSign )
{
    //cout << "getMethodID, object: " << anObj << endl;

    jclass objClass = env->GetObjectClass(anObj);
    //cout << objClass <<endl;
    jmethodID mid = env->GetMethodID(objClass, aMN, aSign);
    return mid;
}

template<class T>
struct Extractor
{
    typedef jobject input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;
};

template<>
struct Extractor<double>
{
    typedef jdouble input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator double()
    {
        //	cout << "extracting primitive double value" << endl;
        return static_cast<double>(obj);
    }
};

template<>
struct Extractor<int32_t>
{
    typedef jint input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator int32_t()
    {
        //	cout << "extracting primitive int32_t value" << endl;
        return static_cast<int32_t>(obj);
    }

    tuple<int32_t> get_pieces() { return forward_as_tuple( static_cast<int32_t>(*this) ); }
};

template<class T>
struct Extractor<jni_array<T>>
{
    typedef typename jni_type<T>::array_type input_type;
    typedef Extractor extract_type;
    typedef jni_array<T> emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator jni_array<T>()
    {
        return jni_array<T>(env, obj);
    }

    tuple<JNIEnv*, input_type> get_pieces()
    {
        return std::forward_as_tuple( env, obj );
    }
    //    static jni_array<T> extract( JNIEnv* env, input_type anObj )
    //    {
        ////	cout << "extracting jni_array<T>" << endl;
        //	return jni_array<T>(env, anObj);
        //    }
};

template<class T, class ET>
struct Emplacer
{
    template<class ... ATs>
    static void emplace_back( std::vector<T>& aV, ATs ... args )
    {
        //	cout << "emplace_back<T,ET>-s" << endl;
        aV.emplace_back( ET( args... ) );
        //	cout << "emplace_back<T,ET>-e" << endl;
    }
};

template<class T>
struct Emplacer<T,T>
{
    template<class ... ATs>
    static void emplace_back( std::vector<T>& aV, ATs ... args )
    {
        //	cout << "QQ" << endl;
        aV.emplace_back( args... );
    }
};

template<class T>
struct Extractor<std::vector<T>>
{
    typedef jni_type<jobject>::array_type input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator std::vector<T>()
    {
        //	cout << "extracting std::vector<T>" << endl;
        int32_t len1 = env->GetArrayLength(obj);

        std::vector<T> res;
        res.reserve( len1 );

        for(int32_t i = 0; i < len1; ++i )
        {
            auto o = static_cast<typename Extractor<T>::input_type>( JNIArray<jobject>::get(env, obj, i) );
            //	    cout << "emplace_back-s: " << i << endl;
            //res.emplace_back( typename Extractor<T>::extract_type (env, o ) );
            Emplacer<T, typename Extractor<T>::emplace_type>::emplace_back( res, env, o );
            //Emplacer<T, T>::emplace_back( res, env, o );
            //	    cout << "emplace_back-e: " << i << endl;
            env->DeleteLocalRef(o);
        }

        return res;
    }

    tuple<std::vector<T>> get_pieces() { return std::forward_as_tuple( this->operator std::vector<T>() ); }
};

template<class A, class B>
struct Extractor<pair<A,B>>
{
    typedef jobject input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator pair<A,B>()
    {
        //	cout << "extracting tuple2: " << anObj << endl;
        // jclass scalaTuple2 = env->FindClass("scala/Tuple2");
        // jboolean yes = env->IsInstanceOf( anObj, scalaTuple2 );
        // if( !yes )
        //     throw(std::runtime_error("anObj isn't scala.Tuple2") );

        //cout << "make-tuple-s" << endl;
        return  std::pair<A,B>( std::piecewise_construct
        , typename Extractor<A>::extract_type (env, GetField<A>::get(env, obj, "_1") ).get_pieces()
        , typename Extractor<B>::extract_type (env, GetField<B>::get(env, obj, "_2") ).get_pieces() );
        //cout << "make-tuple-e" << endl;
        //	return res;
    }
};

template<class A, class B1, class B2>
struct Extractor<pair<A,pair<B1,B2>>>
{
    typedef jobject input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator pair<A,pair<B1,B2>>()
    {
        //	cout << "extracting tuple2: " << anObj << endl;
        // jclass scalaTuple2 = env->FindClass("scala/Tuple2");
        // jboolean yes = env->IsInstanceOf( anObj, scalaTuple2 );
        // if( !yes )
        //     throw(std::runtime_error("anObj isn't scala.Tuple2") );

        //	cout << "make-tuple-s" << endl;
        typedef pair<B1,B2> B;
        return  std::pair<A,pair<B1,B2>>( std::piecewise_construct
        , typename Extractor<A>::extract_type (env, GetField<A>::get(env, obj, "_1") ).get_pieces()
        //				    , typename Extractor<B>::extract_type (env, GetField<B>::get(env, obj, "_2") ).get_pieces()
        , std::forward_as_tuple( static_cast<B1>( typename Extractor<B1>::extract_type (env, GetField<B1>::get(env, GetField<B>::get(env, obj, "_2"), "_1") ) )
        , static_cast<B2>( typename Extractor<B2>::extract_type (env, GetField<B2>::get(env, GetField<B>::get(env, obj, "_2"), "_2") ) ) )
        );
        //	cout << "make-tuple-e" << endl;
        //	return res;
    }
};

template<class A, class B1, class B2>
struct Extractor<pair<pair<B1,B2>,A>>
{
    typedef pair<B1,B2> B;
    typedef jobject input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator pair<B,A>()
    {
        //	cout << "extracting tuple2: " << anObj << endl;
        // jclass scalaTuple2 = env->FindClass("scala/Tuple2");
        // jboolean yes = env->IsInstanceOf( anObj, scalaTuple2 );
        // if( !yes )
        //     throw(std::runtime_error("anObj isn't scala.Tuple2") );

        //	cout << "make-tuple-s" << endl;
        return  std::pair<B,A>( std::piecewise_construct
        //				    , typename Extractor<B>::extract_type (env, GetField<B>::get(env, obj, "_2") ).get_pieces()
        , std::forward_as_tuple( static_cast<B1>( typename Extractor<B1>::extract_type (env, GetField<B1>::get(env, GetField<B>::get(env, obj, "_1"), "_1") ) )
        , static_cast<B2>( typename Extractor<B2>::extract_type (env, GetField<B2>::get(env, GetField<B>::get(env, obj, "_1"), "_2") ) ) )
        , typename Extractor<A>::extract_type (env, GetField<A>::get(env, obj, "_2") ).get_pieces()
        );
        //	cout << "make-tuple-e" << endl;
        //	return res;
    }
};

template<class A1, class A2, class B1, class B2>
struct Extractor<pair<pair<A1,A2>,pair<B1,B2>>>
{
    typedef pair<A1,A2> A;
    typedef pair<B1,B2> B;
    typedef jobject input_type;
    typedef Extractor extract_type;
    typedef Extractor emplace_type;

    JNIEnv* env;
    input_type obj;

    Extractor( JNIEnv* anEnv, input_type anObj ) : env(anEnv), obj(anObj) {}

    operator pair<A,B>()
    {
        //	cout << "extracting tuple2: " << anObj << endl;
        // jclass scalaTuple2 = env->FindClass("scala/Tuple2");
        // jboolean yes = env->IsInstanceOf( anObj, scalaTuple2 );
        // if( !yes )
        //     throw(std::runtime_error("anObj isn't scala.Tuple2") );

        //	cout << "make-tuple-s" << endl;
        return  std::pair<A,B>( std::piecewise_construct
        //				    , typename Extractor<A>::extract_type (env, GetField<A>::get(env, obj, "_1") ).get_pieces()
        //				    , typename Extractor<B>::extract_type (env, GetField<B>::get(env, obj, "_2") ).get_pieces()
        , std::forward_as_tuple( static_cast<A1>( typename Extractor<A1>::extract_type (env, GetField<A1>::get(env, GetField<A>::get(env, obj, "_1"), "_1") ) )
        , static_cast<A2>( typename Extractor<A2>::extract_type (env, GetField<A2>::get(env, GetField<A>::get(env, obj, "_1"), "_2") ) ) )
        , std::forward_as_tuple( static_cast<B1>( typename Extractor<B1>::extract_type (env, GetField<B1>::get(env, GetField<B>::get(env, obj, "_2"), "_1") ) )
        , static_cast<B2>( typename Extractor<B2>::extract_type (env, GetField<B2>::get(env, GetField<B>::get(env, obj, "_2"), "_2") ) ) )
        );
        //	cout << "make-tuple-e" << endl;
        //	return res;
    }
};

template<class T>
struct GetField
{
    static const typename Extractor<T>::input_type get( JNIEnv* env, jobject anObj, const char* aFN )
    {
        jclass objClass = env->GetObjectClass(anObj);
        jfieldID fid = env->GetFieldID( objClass, aFN, "Ljava/lang/Object;");
        // if( fid == nullptr )
        //     throw( std::runtime_error( "fid == nullptr" ) );

        jobject vecO = env->GetObjectField( anObj, fid );

        return static_cast<typename Extractor<T>::input_type>(vecO);
    }
};

template<>
struct GetField<double>
{
    static const typename Extractor<double>::input_type get( JNIEnv* env, jobject anObj, const char* aFN )
    {
        jobject vecO = GetField<jobject>::get( env, anObj, aFN );
        jclass clazz = env->GetObjectClass( vecO );
        jfieldID fid = env->GetFieldID( clazz, "value", "D" );
        jdouble vec1 = env->GetDoubleField( vecO, fid );
        return vec1;
    }
};

template<>
struct GetField<int32_t>
{
    static const typename Extractor<int32_t>::input_type get( JNIEnv* env, jobject anObj, const char* aFN )
    {
        jobject vecO = GetField<jobject>::get( env, anObj, aFN );
        jclass clazz = env->GetObjectClass( vecO );
        jfieldID fid = env->GetFieldID( clazz, "value", "I" );
        jint vec1 = env->GetIntField( vecO, fid );
        return vec1;
    }
};
