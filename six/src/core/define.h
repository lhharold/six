#ifndef __SIX_DEFINE_H_INCLUDE__
#define __SIX_DEFINE_H_INCLUDE__

#define OS_PLATFORM_WIN32   0
#define OS_PLATFORM_IPHONE  1
#define OS_PLATFORM_MAC		2
#define OS_PLATFORM_LINUX	3
#define OS_PLATFORM_VXWORKS 4
#define OS_PLATFORM_ANDROID 5

#ifdef WIN32
#define OS_PLATFORM OS_PLATFORM_WIN32
#elif defined _MAC
#define OS_PLATFORM OS_PLATFORM_MAC
#elif defined OS_ANDROID
#define OS_PLATFORM OS_PLATFORM_ANDROID
#else
#define OS_PLATFORM OS_PLATFORM_IPHONE
#endif //WIN32

// system include
#if OS_PLATFORM == OS_PLATFORM_WIN32
#include <windows.h>
#endif

// c/c++ standard lib include
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

//temp using std
#include <algorithm>
#include <string>
#include <vector>
#include <map>

using std::string;
using std::vector;
using std::map;

namespace six {
	typedef unsigned char		u8;
	typedef signed char			s8;
	typedef unsigned short		u16;
	typedef signed short		s16;
	typedef unsigned int		u32;
	typedef signed int			s32;
#if defined(_MSC_VER)
	typedef unsigned __int64	u64;
	typedef signed __int64		s64;
#elif defined(__GNUC__) || defined(__clang__)
	typedef unsigned long long u64;
	typedef signed long long   s64;
#endif //
	typedef float				f32;
	typedef double				f64;

	typedef string String;

	#define NEW new
	#define SAFE_DEL(p)			do{ if (p) { delete (p); (p) = NULL; } }while(0)
	#define SAFE_DEL_ARRAY(p)	do{ if (p) { delete[] (p); (p) = NULL; } }while(0)

	#define PRINTF printf
	#define SSCANF sscanf

	#define MEMCPY memcpy
	#define MEMSET memset
	#define STRLEN strlen
	#define STRCPY strcpy
	#define STRCHR strchr
	#define STRSTR strstr

	#define FOPEN	fopen
	#define FREAD	fread
	#define FTELL	ftell
	#define FSEEK	fseek
	#define FWRITE	fwrite
	#define FCLOSE	fclose
	#define FREMOVE ::remove
	#define FEOF	feof

	#define ASSERT assert

#ifndef NULL
  #define NULL 0
#endif

#if 0
#if 0
	#define CLAMP(val, min, max)  ( (val) > (max) ? (max) : ((val)<(min)?(min):(val)) )
	#define MIN(x,y)		          ((x)<(y)?(x):(y))
	#define MAX(x,y)		          ((x)>(y)?(x):(y))
	#define ABS(x)                ((x) > 0 ? (x) : (-(x)))
#else
    static template<class T> inline T CLAMP(T val, T min, T max) {return val > max ? max : (val < min ? min : val);}
    static template<class T> inline T MIN(T x, T y) {return x < y ? x : y;}
    static template<class T> inline T MAX(T x, T y) {return x > y ? x : y;}
    static template<class T> inline T ABS(T x) {return x > 0 ? x : -x;}
#endif
	#define F_PRECISION	1e06
	inline bool F_ZERO(f32 x, f32 precision = F_PRECISION) { return ABS(x) < precision; };
	inline bool F_LESS_ZERO(f32 x, f32 precision = F_PRECISION) { return x < -precision; };
	inline bool F_MORE_ZERO(f32 x, f32 precision = F_PRECISION) { return x > precision; };
	inline bool F_EQUAL(f32 a, f32 b, f32 precision = F_PRECISION) { return F_ZERO(a-b, precision); };
	inline bool F_MORE(f32 a, f32 b, f32 precision = F_PRECISION) { return F_MORE_ZERO(a-b, precision); };
	inline bool F_LESS(f32 a, f32 b, f32 precision = F_PRECISION) { return F_LESS_ZERO(a-b, precision); };
	inline bool F_MORE_EQUAL(f32 a, f32 b, f32 precision = F_PRECISION) { return !F_LESS(a, b, precision); };
	inline bool F_LESS_EQUAL(f32 a, f32 b, f32 precision = F_PRECISION) { return !F_MORE(a, b, precision); };

  inline f32 F_ROUND(f32 x) {return x > 0.f ? int(x+0.5f) : int(x-0.f);}
#endif
}

#pragma warning(disable: 4996)

#endif //__SIX_DEFINE_H_INCLUDE__