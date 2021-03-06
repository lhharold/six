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
#include <time.h>
#endif

// c/c++ standard lib include
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

//temp using std
#include <algorithm>
#include <string>
#include <vector>
#include <set>
#include <deque>
#include <map>
#include <list>

using std::string;
using std::vector;
using std::map;
using std::multimap;
using std::set;
using std::deque;
using std::list;

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
	typedef float				    f32;
	typedef double				  f64;

	static const u32 U32_MAX_VALUE = 0xffffffff;
	static const u32 U32_MIN_VALUE = 0;

	static const s32 S32_MAX_VALUE =  0x7FFFFFFF;
	static const s32 S32_MIN_VALUE = -S32_MAX_VALUE-1;

	static const u16 U16_MAX_VALUE = 0xffff;
	static const u16 U16_MIN_VALUE = 0;

	static const s16 S16_MAX_VALUE =  0x7FFF;
	static const s16 S16_MIN_VALUE = -S16_MAX_VALUE-1;

	static const u8	 U8_MAX_VALUE  =  0xff;
	static const u8  U8_MIN_VALUE  =  0;

	static const s8  S8_MAX_VALUE  =  0x7f;
	static const s8  S8_MIN_VALUE  = -S8_MAX_VALUE-1;

	static const f32 F32_MAX_VALUE = 3.402823466e+38F;
	static const f32 F32_MIN_VALUE = 1.192092896e-07F;

	typedef string String;

  template<typename T>
  class Vector : public std::vector<T> {};
  template<typename K, typename T>
  class Map : public std::map<K, T> {};
  template<typename K, typename T>
  class Mutimap : public std::multimap<K, T> {};
  template<typename T>
  class Set : public std::set<T> {};
  template<typename T>
  class Deque : public std::deque<T> {};
  template<typename T>
  class List : public std::list<T> {};

	#define NEW new
	#define SAFE_DEL(p)			do{ if (p) { delete (p); (p) = NULL; } }while(0)
	#define SAFE_DEL_ARRAY(p)	do{ if (p) { delete[] (p); (p) = NULL; } }while(0)

	#define PRINTF printf
	#define SSCANF sscanf
  #define SPRINTF sprintf

	#define MEMCPY memcpy
	#define MEMSET memset
	#define STRLEN strlen
	#define STRCPY strcpy
	#define STRCHR strchr
	#define STRSTR strstr
  #define STRCMP strcmp
  #define STRCAT strcat

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

//#if 1
//#if 0
//	#define CLAMP(val, min, max)  ( (val) > (max) ? (max) : ((val)<(min)?(min):(val)) )
//	#define MIN(x,y)		          ((x)<(y)?(x):(y))
//	#define MAX(x,y)		          ((x)>(y)?(x):(y))
//	#define ABS(x)                ((x) > 0 ? (x) : (-(x)))
//#else
//    static template<class T> inline T CLAMP(T val, T min, T max) {return val > max ? max : (val < min ? min : val);}
//    static template<class T> inline T MIN(T x, T y) {return x < y ? x : y;}
//    static template<class T> inline T MAX(T x, T y) {return x > y ? x : y;}
//    static template<class T> inline T ABS(T x) {return x > 0 ? x : -x;}
//#endif
//	#define F_PRECISION	1e06
//	inline bool F_ZERO(f32 x, f32 precision = F_PRECISION) { return ABS(x) < precision; };
//	inline bool F_LESS_ZERO(f32 x, f32 precision = F_PRECISION) { return x < -precision; };
//	inline bool F_MORE_ZERO(f32 x, f32 precision = F_PRECISION) { return x > precision; };
//	inline bool F_EQUAL(f32 a, f32 b, f32 precision = F_PRECISION) { return F_ZERO(a-b, precision); };
//	inline bool F_MORE(f32 a, f32 b, f32 precision = F_PRECISION) { return F_MORE_ZERO(a-b, precision); };
//	inline bool F_LESS(f32 a, f32 b, f32 precision = F_PRECISION) { return F_LESS_ZERO(a-b, precision); };
//	inline bool F_MORE_EQUAL(f32 a, f32 b, f32 precision = F_PRECISION) { return !F_LESS(a, b, precision); };
//	inline bool F_LESS_EQUAL(f32 a, f32 b, f32 precision = F_PRECISION) { return !F_MORE(a, b, precision); };
//
//  inline f32 F_ROUND(f32 x) {return x > 0.f ? int(x+0.5f) : int(x-0.f);}
//#endif
}

#pragma warning(disable: 4996)

#endif //__SIX_DEFINE_H_INCLUDE__