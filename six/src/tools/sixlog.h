#ifndef __SIX_SIXLOG_H_INCLUDE__
#define __SIX_SIXLOG_H_INCLUDE__

#include "logtracer.h"
#if defined(_LOG4CPP)
#include <log4cpp/Category.hh>
#endif

namespace six {
  struct Logexport {
    static void funInitializeLog4cpp(const char* logfile);
    static void funReleaseLog4cpp();
    static void suck(const char* fmt, ...);
#if defined(_LOG4CPP)
    static log4cpp::Category & GetLogCategory(const char * categoryName);
#endif
  };
#define InitLog4cpp Logexport::funInitializeLog4cpp
#define ReleaseLog4cpp Logexport::funReleaseLog4cpp

#if defined(_LOG4CPP)
# define DECLARE_STATIC_LOG() static log4cpp::Category& log
# define DEFINE_STATIC_LOG(className) log4cpp::Category& className::log = Logexport::GetLogCategory(#className)
#else
# define DECLARE_STATIC_LOG()
# define DEFINE_STATIC_LOG(className)
#endif

#if defined(_LOG4CPP)
# ifdef WIN32
#   define MakePrefix             String(__FUNCTION__).append("() - ").c_str()
#   define LogDebug               (LogDebuger(log, MakePrefix))
#   define LogInfo                (LogInfoer(log, MakePrefix))
#   define LogNotice              (LogNoticer(log, MakePrefix))
#   define LogError               (LogErrorer(log, MakePrefix))
# elif !defined(WIN32)
#   define MakePrefix(fmt)        String(__FILE__).append("::").append(__FUNCTION__).append("() - ").append(fmt).c_str()
#   define LogDebug(fmt, ...)     log.debug(MakePrefix(fmt), ##__VA_ARGS__)
#   define LogInfo(fmt, ...)      log.info(MakePrefix(fmt), ##__VA_ARGS__)
#   define LogNotice(fmt, ...)    log.notice(MakePrefix(fmt), ##__VA_ARGS__)
#   define LogError(fmt, ...)     log.error(MakePrefix(fmt), ##__VA_ARGS__)
# endif
#else
# define LogDebug               Logexport::suck
# define LogInfo                Logexport::suck
# define LogNotice              Logexport::suck
# define LogError               Logexport::suck
#endif
}
#endif //__SIX_SIXLOG_H_INCLUDE__