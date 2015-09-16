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
#include <string>
#include <assert.h>

using std::string;

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



	typedef u32					UInt;
	typedef s32					Int;
	typedef f32					Float;



	#define NEW new
	#define SAFE_DEL(p)			do{ if (p) { delete (p); (p) = NULL; } }while(0)
	#define SAFE_DEL_ARRAY(p)	do{ if (p) { delete[] (p); (p) = NULL; } }while(0)

	#define MEMCPY memcpy
	#define STRCPY strcpy

	#define ASSERT assert

	typedef string String;
}

#endif //__SIX_DEFINE_H_INCLUDE__