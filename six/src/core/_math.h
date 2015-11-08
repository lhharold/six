#ifndef __SIX_MATH_H_INCLUDE__
#define __SIX_MATH_H_INCLUDE__

namespace six {
  class Math{
  public:
    static const f32 f_precision;
    static const f32 pi;
    static const f32 invpi;
    static const f32 inv180;
    static const f32 rad2deg;
    static const f32 deg2rad;
    static const f32 piDegree;

    static inline f32 sqrt(f32 v) { return sqrtf(v);}
    static inline f32 invsqrt(f32 v) {return 1.f/sqrtf(v);}
    static inline bool isNan(f32 v) {return v != v;}

    static inline f32 Deg2Rad(f32 x) {return x*deg2rad;}
    static inline f32 Rad2Deg(f32 x) {return x*rad2deg;}

    static inline f32 asin(f32 x) {return ::asin(x);}
    static inline f32 cos(f32 x) {return ::cos(x);}
    static inline f32 acos(f32 x) {return ::acos(x);}
    static inline f32 tan(f32 x) {return ::tan(x);}
    static inline f32 atan(f32 x) {return ::atan(x);}

#if 0
    static inline f32 sin(f32 x) {sin(x);}
    static template<typename T> inline T clamp(T val, T min, T max) {return val > max ? max : (val < min ? min : val);}
    static template<typename T> inline T min(T x, T y) {return x < y ? x : y;}
    static template<typename T> inline T max(T x, T y) {return x > y ? x : y;}
    static template<typename T> inline T abs(T x) {return x > 0 ? x : -x;}
  	static template<typename T> inline void swap(T& a, T& b) {T tmp = a; a = b; b = tmp;}
#endif

    static inline bool f_zero(f32 x, f32 precision = f_precision) { return abs(x) < precision; };
  	static inline bool f_less_zero(f32 x, f32 precision = f_precision) { return x < -precision; };
  	static inline bool f_more_zero(f32 x, f32 precision = f_precision) { return x > precision; };
  	static inline bool f_equal(f32 a, f32 b, f32 precision = f_precision) { return f_zero(a-b, precision); };
  	static inline bool f_more(f32 a, f32 b, f32 precision = f_precision) { return f_more_zero(a-b, precision); };
  	static inline bool f_less(f32 a, f32 b, f32 precision = f_precision) { return f_less_zero(a-b, precision); };
  	static inline bool f_more_equal(f32 a, f32 b, f32 precision = f_precision) { return !f_less(a, b, precision); };
  	static inline bool f_less_equal(f32 a, f32 b, f32 precision = f_precision) { return !f_more(a, b, precision); };
    static inline int f_round(f32 x) {return x > 0.f ? int(x+0.5f) : int(x-0.f);}
  };
#if 0
  #define CLAMP         Math::clamp
  #define MIN           Math::min
  #define MAX           Math::max
  #define ABS           Math::abs
  #define SWAP          Math::swap
#else
	#define CLAMP(val, min, max)  ( (val) > (max) ? (max) : ((val)<(min)?(min):(val)) )
	#define MIN(x,y)		          ((x)<(y)?(x):(y))
	#define MAX(x,y)		          ((x)>(y)?(x):(y))
	#define ABS(x)                ((x) > 0 ? (x) : (-(x)))
  template<typename T> inline void _swap(T& a, T& b) {T tmp = a; a = b; b = tmp;}
  #define SWAP                  _swap
#endif
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