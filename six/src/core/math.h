#ifndef __SIX_MATH_H_INCLUDE__
#define __SIX_MATH_H_INCLUDE__

namespace six {
  class Math{
  public:
  	static const f32 f_precision = 1e06;

    static inline f32 sqrt(f32 v) { return sqrtf(v);}
    static inline f32 invsqrt(f32 v) {return 1.f/sqrtf(v);}
    static inline bool isNan(f32 v) {return v != v;}

    static template<class T> inline T clamp(T val, T min, T max) {return val > max ? max : (val < min ? min : val);}
    static template<class T> inline T min(T x, T y) {return x < y ? x : y;}
    static template<class T> inline T max(T x, T y) {return x > y ? x : y;}
    static template<class T> inline T abs(T x) {return x > 0 ? x : -x;}
  	static template<class T> inline void swap(T& a, T& b) {T tmp = a; a = b; b = tmp;}

    static inline bool f_zero(f32 x, f32 precision = f_precision) { return abs(x) < precision; };
  	static inline bool f_less_zero(f32 x, f32 precision = f_precision) { return x < -precision; };
  	static inline bool f_more_zero(f32 x, f32 precision = f_precision) { return x > precision; };
  	static inline bool f_equal(f32 a, f32 b, f32 precision = f_precision) { return f_zero(a-b, precision); };
  	static inline bool f_more(f32 a, f32 b, f32 precision = f_precision) { return f_more_zero(a-b, precision); };
  	static inline bool f_less(f32 a, f32 b, f32 precision = f_precision) { return f_less_zero(a-b, precision); };
  	static inline bool f_more_equal(f32 a, f32 b, f32 precision = f_precision) { return !f_less(a, b, precision); };
  	static inline bool f_less_equal(f32 a, f32 b, f32 precision = f_precision) { return !f_more(a, b, precision); };
    static inline f32 f_round(f32 x) {return x > 0.f ? int(x+0.5f) : int(x-0.f);}
  };
  #define CLAMP         Math::clamp
  #define MIN           Math::min
  #define MAX           Math::max
  #define ABS           Math::abs
  #define SWAP          Math::swap
  #define F_PRECISION  	Math::f_precision
  #define F_ZERO        Math::f_zero
  #define F_LESS_ZERO   Math::f_less_zero
  #define F_MORE_ZERO   Math::f_more_zero
  #define F_EQUAL       Math::f_equal
  #define F_MORE        Math::f_more
  #define F_LESS        Math::f_less
  #define F_MORE_EQUAL  Math::f_more_equal
  #define F_LESS_EQUAL  Math::f_less_equal
  #define F_ROUND       Math::f_round
}

#endif //__SIX_MATH_H_INCLUDE__